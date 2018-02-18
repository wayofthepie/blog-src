---
title: "Containerizing The JVM: Basic Memory Overview"
tags: jvm,container
---

# Introduction
For the past few years where I work has been running JVM applications on [Cloud Foundry](https://www.cloudfoundry.org/). 
We have come across many issues in relation to memory when running JVM's within memory 
bounded containers. 

Cloud foundry runs applications inside [garden containers](https://www.cloudfoundry.org/blog/cloud-foundry-containers-difference-warden-docker-garden/). 
These containers have memory limits set on application creation, they use cgroups to impose these limits
just as docker does - if the sum of the memory allocated to each process within the container
exceeds the limit, the kernel _oom-killer_ will kick in and kill the container. The main issue we've seen with JVM 
applications is non-heap memory. Tracking and tuning the heap is trivial in comparison to tracking 
and tuning non-heap memory usage - by non-heap I don't just mean _Metaspace_, but also 
direct buffers, code cache, stack sizes, and so on.

I've been wanting to do a series of posts
on the topic of JVM's in containers for a while, I finally put the time aside to start!

## What is this post about?
The goal of this post is to give an overview of the heap and non-heap 
memory regions of the JVM - with some small introduction to both - and also to 
show what happens in the event of a heap/non-heap memory issue within a `docker` container. I assume some
basic knowledge of Java, the JVM, docker and linux. You will need docker and openjdk 8 installed on 
a linux system (I used ubuntu 16.04 to write this post). 

# Containerizing a java app
To start I'm going to keep things super simple. Let's build a program
which prints "Hello world!" and waits forever:

```{.java}
// HelloWorld.java
public class HelloWorld {
    public static void main(String[] args) throws Exception {
        System.out.println("Hello world!");
        System.in.read();
    }
}
```

Now, a simple Dockerfile:

```{.dockerfile .nginx}
FROM openjdk:8-jdk
ADD HelloWorld.java .
RUN javac HelloWorld.java
ENTRYPOINT java HelloWorld
```

With that we can build and launch our application in a container:

```bash
$ docker build --tag jvm-test . 
$ docker run -ti --rm --name hello-jvm jvm-test
Hello world!

```
You can use CTRL-C to kill the container when you are done. Right, now we have a simple 
program running, what can we do? Let's analyze the JVM.

## Basic JVM analysis
Lets get a list what objects we have on the heap within our application. 
First, get into the container (assuming it's still running from above) and get the JVM processes PID.

```bash
$ docker exec -ti hello-jvm bash
root@5f20ae043968:/ $ ps aux|grep [j]ava
root         1  0.1  0.0   4292   708 pts/0    Ss+  12:27   0:00 /bin/sh -c java HelloWorld
root         7  0.2  0.1 6877428 23756 pts/0   Sl+  12:27   0:00 java HelloWorld
```
From the above, we see the PID is 7. For analysis, the openjdk comes with a number of tools.
`jmap` is one such tool which allows us to view heap information about
a JVM process.
To get a list of objects, their number of instances and the space they take up in the heap
you can use `jmap -histo <JVM_PID>`.

```bash
root@5f20ae043968:/ $ jmap -histo 7

 num     #instances         #bytes  class name
----------------------------------------------
   1:           422        2256744  [I
   2:          1600         141520  [C
   3:           364          58560  [B
   4:           470          53544  java.lang.Class
   5:          1204          28896  java.lang.String
   6:           551          28152  [Ljava.lang.Object;
   7:           110           7920  java.lang.reflect.Field
   8:           258           4128  java.lang.Integer
   9:            97           3880  java.lang.ref.SoftReference
  10:           111           3552  java.util.Hashtable$Entry
  11:           133           3192  java.lang.StringBuilder
  12:             8           3008  java.lang.Thread
  13:            75           2400  java.io.File
  14:            54           2080  [Ljava.lang.String;
  15:            38           1824  sun.util.locale.LocaleObjectCache$CacheEntry
  16:            12           1760  [Ljava.util.Hashtable$Entry;
  17:            55           1760  java.util.concurrent.ConcurrentHashMap$Node
  18:            27           1728  java.net.URL
  19:            20           1600  [S
  ...
  222:             1             16  sun.reflect.ReflectionFactory
Total          6583        2642792
```
As you can see above there are 6583 instances of a mixture of 222 different classes, taking
up over 2.6MB of the heap, for our simple HelloWorld program! When I first saw this it raised
a lot of questions - what is `[I`, why is there a `java.lang.String` and a `[Ljava.lang.String`?

## What are all these classes?
The single letter class names you see above are all documented under 
[Class.getName()](https://docs.oracle.com/javase/6/docs/api/java/lang/Class.html#getName()).

---------------------------------------
|   Encoding   |   Element Type  |
|--------------|-----------------|
| Z            | boolean         |
| B            | byte            |
| C            | char            |
| L*className* | class/interface |
| D            | double          |
| F            | float           |
| I            | int             |
| J            | long            |
| S            | short           |

Note the above are all _native_ types. If you look back to the `jmap` output, the 
first few instances all have `[` prefixing them - e.g. `[I`. `[` denotes a 1 dimensional 
array of the type proceeding it - `[I` denotes an array of `int` e.g. `new int[3]`. 
`[[I` denotes a 2D array, `new int[2][3]` and so on. Also in the `jmap` output above were instances 
of `[L.java.lang.String` which is just an array of String's - `new String[3]`.

To see this for yourself:

```java
// InstanceName.java
public class InstanceName {
    public static void main(String[] args) throws Exception {
      int[] is = new int[3];
      System.out.println(is.getClass().getName());

      boolean[][][] bs = new boolean[2][5][4];
      System.out.println(bs.getClass().getName());

      String[] ss = new String[3];
      System.out.println(ss.getClass().getName());
    } 
}
```
Compiling and running this we get:

```bash
$ javac InstanceName.java 
$ java InstanceName 
[I
[[[Z
[Ljava.lang.String;
```
That's a quick overview of one way to look at what's loaded on the heap. I mentioned other memory 
regions in the JVM earlier, what are these?

# Heap and Non-Heap memory
The JVM can be divided into many different memory segments (segments/regions/areas, I'll use these 
words interchangeably, but generally they mean the same thing), 
if we take a high level view first we have two segments - memory used for objects on the heap and 
non-heap memory. 

If we zoom in, the heap has different areas within 
which we can talk about, depending on what we want to discuss - there is the Eden space, where most
new objects are initially created, the Survivor space, where objects go if they survive an Eden space
garbage collection (GC) and the Old Generation which contains objects that have lived in 
Survivor space for a while. Specifically, it contains objects that have been initialized - e.g. 
`List<String> s = new ArrayList<String>();` will create an `ArrayList` object on the heap, and `s`
will point to this.

In the previous section I ran through what objects are loaded into the heap for our HelloWorld program,
so what about non-heap memory?

## Non-Heap Memory 
If you have ever written a non-trivial java application with jdk8 you have probably heard of _Metaspace_.
This is an example of non-heap memory. It's where the JVM will store class definitions, static variables, methods, 
classloaders and other metadata. But there are many other non-heap memory regions the JVM will use.
Let's list them! 

To do so, first we need to enable native memory tracking in our java app:

```{.dockerfile .nginx}
FROM openjdk:8-jdk
ADD HelloWorld.java .
RUN cat HelloWorld.java
RUN javac HelloWorld.java
ENTRYPOINT java -XX:NativeMemoryTracking=detail HelloWorld
```
Now build and re-run:

```bash
$ docker build --tag jvm-test . 
$ docker run -ti --rm --name hello-jvm jvm-test
Hello world!
```
In another terminal, exec into the container and get a summary of overall memory usage 
with `jcmd`'s `VM.native_memory` command:

```bash
$ docker exec --privileged -ti hello-jvm bash
root@aa5ae77e1305:/ $ jcmd 
33 sun.tools.jcmd.JCmd
7 HelloWorld

root@aa5ae77e1305:/ $ jcmd 7 VM.native_memory summary
7:

Native Memory Tracking:

Total: reserved=5576143KB, committed=1117747KB
-                 Java Heap (reserved=4069376KB, committed=920064KB)
                            (mmap: reserved=4069376KB, committed=920064KB) 
 
-                     Class (reserved=1066121KB, committed=14217KB)
                            (classes #405)
                            (malloc=9353KB #178) 
                            (mmap: reserved=1056768KB, committed=4864KB) 
 
-                    Thread (reserved=20646KB, committed=20646KB)
                            (thread #21)
                            (stack: reserved=20560KB, committed=20560KB)
                            (malloc=62KB #110) 
                            (arena=23KB #40)
 
-                      Code (reserved=249655KB, committed=2591KB)
                            (malloc=55KB #346) 
                            (mmap: reserved=249600KB, committed=2536KB) 
 
-                        GC (reserved=159063KB, committed=148947KB)
                            (malloc=10383KB #129) 
                            (mmap: reserved=148680KB, committed=138564KB) 
 
-                  Compiler (reserved=134KB, committed=134KB)
                            (malloc=3KB #37) 
                            (arena=131KB #3)
 
-                  Internal (reserved=9455KB, committed=9455KB)
                            (malloc=9423KB #1417) 
                            (mmap: reserved=32KB, committed=32KB) 
 
-                    Symbol (reserved=1358KB, committed=1358KB)
                            (malloc=902KB #85) 
                            (arena=456KB #1)
 
-    Native Memory Tracking (reserved=161KB, committed=161KB)
                            (malloc=99KB #1559) 
                            (tracking overhead=61KB)
 
-               Arena Chunk (reserved=175KB, committed=175KB)
                            (malloc=175KB) 
 

```
A lot more regions than just the heap! Our hello world program just got even more complex...

What does all this mean? [^1]

  * **Java Heap** : heap memory. 
  * **Class** : is the _Metaspace_ region we previously spoke about.
  * **Thread** : is the space taken up by threads on this JVM's.
  * **Code** : is the code cache - this is used by the JIT to cache compiled code.
  * **GC** : space used by the garbage collector.
  * **Compiler** : space used by the JIT when generating code.
  * **Symbols** : this is for symbols, by which I believe field names, method signatures fall under. [^2]
  * **Native Memory Tracking** : memory used by the native memory tracker itself.
  * **Arena Chunk** : not entirely sure what this gets used for. [^3]

## Practical memory issues
Ok, so why should you care about any of the above? Let's create an app that eats a tonne of memory.

```java
// MemEater.java
import java.util.Vector;

public class MemEater {
    public static final void main(String[] args) throws Exception {
        Vector<byte[]> v = new Vector<byte[]>();
        for (int i = 0; i < 400; i++) {
            byte[] b = new byte[1048576]; // allocate 1 MiB
            v.add(b);
        }
        System.out.println(v.size());
        Thread.sleep(10000);
    }
}
```
This will create a `Vector` which contains 400 byte arrays of size 1 MiB [^4], so this will use ~400MiB memory on the heap.
It will then sleep for 10 seconds so we can get the memory usage easily while it runs.
Let's constrain the heap to 450MiB and run this locally we can see the actual memory usage of the process. 
RSS Resident Set Size [^5] is how this is measured, note that this value also contains pages mapped from _shared memory_, 
but we can gloss over that for this post.

So, lets compile our app, run in the background and get its RSS:

```bash
$ javac MemEater.java 
$ nohup java -Xms450M -Xmx450M MemEater & 
$ ps aux | awk 'NR==1; /[M]emEater/' 
USER       PID %CPU %MEM    VSZ   RSS TTY      STAT START   TIME COMMAND
chaospie 18019 10.5  3.0 3138368 494448 pts/19 Sl   16:06   0:00 java -Xms450M -Xmx450M MemEater
```
In total, the JVM process needs about 500 MiB to run (RSS is 494448 KiB). What happens if we set the heap to a size lower than it needs?

```bash
$ java -Xms400M -Xmx400M MemEater
Exception in thread "main" java.lang.OutOfMemoryError: Java heap space
	at MemEater.main(MemEater.java:7)
```
If you have used java (or any JVM language) before, you have more than likely come across this. It means that the 
JVM ran out of heap space to allocate objects. There are quite a few other types of `OutOfMemoryError` the JVM can 
throw in certain situations [^6], but I won't go into more detail right now.

Now we know what happens if the JVM does not have enough heap space, what about the case where you are running 
in a container and hit the overall memory limit for that container?

The simplest way to reproduce this is to package up our `MemEater` program into a docker image and run it with 
less memory than it needs.

```{.dockerfile .nginx}
FROM openjdk:8-jdk
ADD MemEater.java .
RUN cat MemEater.java
RUN javac MemEater.java
ENTRYPOINT java -Xms450M -Xmx450M MemEater
```
Again, we need to build the image. However this time when we are running we limit the memory the 
container is allowed to use to 5M:

```
$ docker build --tag jvm-test .
$ docker run -ti --rm --memory 5M --memory-swappiness 0 --name memeater jvm-test
WARNING: Your kernel does not support swap limit capabilities or the cgroup is not mounted. Memory limited without swap.
Killed
```
After a few seconds you should see the output above, `Killed`. What happened? Before we dive into that, lets have
a look at the `--memory` and `--memory-swappiness` flags used by `docker`.

### Limiting memory with docker
Lets digress for a second, and look at the two docker flags I used above for controlling memory settings [^7].
First, for these flags to work, your kernel will need to have cgroup support enabled and the following boot 
parameters set (assuming `grub`):

```bash
$ cat /etc/default/grub
...
GRUB_CMDLINE_LINUX="cgroup_enable=memory swapaccount=1"
...
```

`--memory` sets an upper bound on the sum of all processes memory usage within a container,
the smallest this can go is 4MiB, above we set it to 5m which is 5MiB. When this is set, 
the containers `cgroup` `memory.limit_in_bytes` is set to the value. I can't
find the code that does this in `docker`, however we can see it as follows:

```bash
$ docker run -d --rm --memory 500M --memory-swappiness 0 --name memeater jvm-test 
WARNING: Your kernel does not support swap limit capabilities or the cgroup is not mounted. Memory limited without swap.
812dbc3417eacdaf221c2f0c93ceab41f7626dca17f959298a5700358f931897
$ CONTAINER_ID=`docker ps --no-trunc | awk '{if (NR!=1) print $1}'`
$ echo $CONTAINER_ID
812dbc3417eacdaf221c2f0c93ceab41f7626dca17f959298a5700358f931897
$ cat /sys/fs/cgroup/memory/docker/${CONTAINER_ID}/memory.swappiness 
0
$ cat /sys/fs/cgroup/memory/docker/${CONTAINER_ID}/memory.limit_in_bytes
524288000

# Again, this time without limits to see the difference
$ docker run -d --rm --name memeater jvm-test 
d3e25423814ee1d79759aa87a83d416d63bdb316a305e390c2b8b98777484822
$ CONTAINER_ID=`docker ps --no-trunc | awk '{if (NR!=1) print $1}'`
$ echo $CONTAINER_ID
d3e25423814ee1d79759aa87a83d416d63bdb316a305e390c2b8b98777484822
$ cat /sys/fs/cgroup/memory/docker/${CONTAINER_ID}/memory.swappiness 
60
$ cat /sys/fs/cgroup/memory/docker/${CONTAINER_ID}/memory.limit_in_bytes
9223372036854771712
```

<div class="alert alert-warning">
Note the `WARNING`, I'm not entirely sure why this appears as swap support is enabled, and seems to work. 
You can ignore this for now.
</div>
`--memory-swappiness` sets the _swappiness_ level of the cgroup herarchy the container runs in. 
This maps directly to the cgroup setting **memory.swappiness** (at least in version 17.12 of _docker_ [^8] ) as seen above.
Setting this to 0 disables swap for the container. 

## What kills the container?
So, why was the container killed? Lets run it again:

```{.dockerfile .nginx}
$ docker run -ti --rm --memory 5M --memory-swappiness 0 --name memeater jvm-test
WARNING: Your kernel does not support swap limit capabilities or the cgroup is not mounted. Memory limited without swap.
Killed
```
To see the cause of this kill, run `journalctl -k` and search for `oom-killer`, you should see 
logs like the following:

```bash
$ journalctl -k
...
Feb 18 17:34:47  kernel: java invoked oom-killer: gfp_mask=0x14000c0(GFP_KERNEL), nodemask=(null),  order=0, oom_score_adj=0
Feb 18 17:34:47  kernel: java cpuset=35f18c48d432510c76e76f2e7a962e64a1372de1dc4abd830417263907bea6e0 mems_allowed=0
Feb 18 17:34:47  kernel: CPU: 0 PID: 16432 Comm: java Tainted: G           OE   4.13.0-32-generic #35~16.04.1-Ubuntu
Feb 18 17:34:47  kernel: Hardware name: Dell Inc. Precision 5520/0R6JFH, BIOS 1.3.3 05/08/2017
Feb 18 17:34:47  kernel: Call Trace:
Feb 18 17:34:47  kernel:  dump_stack+0x63/0x8b
Feb 18 17:34:47  kernel:  dump_header+0x97/0x225
Feb 18 17:34:47  kernel:  ? mem_cgroup_scan_tasks+0xc4/0xf0
Feb 18 17:34:47  kernel:  oom_kill_process+0x219/0x420
Feb 18 17:34:47  kernel:  out_of_memory+0x11d/0x4b0
Feb 18 17:34:47  kernel:  mem_cgroup_out_of_memory+0x4b/0x80
Feb 18 17:34:47  kernel:  mem_cgroup_oom_synchronize+0x325/0x340
Feb 18 17:34:47  kernel:  ? get_mem_cgroup_from_mm+0xa0/0xa0
Feb 18 17:34:47  kernel:  pagefault_out_of_memory+0x36/0x7b
Feb 18 17:34:47  kernel:  mm_fault_error+0x8f/0x190
Feb 18 17:34:47  kernel:  ? handle_mm_fault+0xcc/0x1c0
Feb 18 17:34:47  kernel:  __do_page_fault+0x4c3/0x4f0
Feb 18 17:34:47  kernel:  do_page_fault+0x22/0x30
Feb 18 17:34:47  kernel:  ? page_fault+0x36/0x60
Feb 18 17:34:47  kernel:  page_fault+0x4c/0x60
Feb 18 17:34:47  kernel: RIP: 0033:0x7fdeafb0fe2f
Feb 18 17:34:47  kernel: RSP: 002b:00007fdeb0e1db80 EFLAGS: 00010206
Feb 18 17:34:47  kernel: RAX: 000000000001dff0 RBX: 00007fdea802d490 RCX: 00007fdeac17b010
Feb 18 17:34:47  kernel: RDX: 0000000000003bff RSI: 0000000000075368 RDI: 00007fdeac17b010
Feb 18 17:34:47  kernel: RBP: 00007fdeb0e1dc20 R08: 0000000000000000 R09: 0000000000000000
Feb 18 17:34:47  kernel: R10: 0000000000000022 R11: 0000000000000246 R12: 0000000000000000
Feb 18 17:34:47  kernel: R13: 00007fdeb0e1db90 R14: 00007fdeafff851b R15: 0000000000075368
Feb 18 17:34:47  kernel: Task in /docker/35f18c48d432510c76e76f2e7a962e64a1372de1dc4abd830417263907bea6e0 killed as a result of limit of /docker/35f18c48d432510c76e76f2e7a962e64a137
Feb 18 17:34:47  kernel: memory: usage 5120kB, limit 5120kB, failcnt 69
Feb 18 17:34:47  kernel: memory+swap: usage 0kB, limit 9007199254740988kB, failcnt 0
Feb 18 17:34:47  kernel: kmem: usage 1560kB, limit 9007199254740988kB, failcnt 0
Feb 18 17:34:47  kernel: Memory cgroup stats for /docker/35f18c48d432510c76e76f2e7a962e64a1372de1dc4abd830417263907bea6e0: cache:176KB rss:3384KB rss_huge:0KB shmem:144KB mapped_fil
Feb 18 17:34:47  kernel: [ pid ]   uid  tgid total_vm      rss nr_ptes nr_pmds swapents oom_score_adj name
Feb 18 17:34:47  kernel: [16360]     0 16360     1073      178       8       3        0             0 sh
Feb 18 17:34:47  kernel: [16426]     0 16426   609544     3160      47       4        0             0 java
Feb 18 17:34:47  kernel: Memory cgroup out of memory: Kill process 16426 (java) score 2508 or sacrifice child
Feb 18 17:34:47  kernel: Killed process 16426 (java) total-vm:2438176kB, anon-rss:3200kB, file-rss:9440kB, shmem-rss:0kB
...
```
The kernels OOM killer killed the application because it violated it's `cgroup` memory limit.
From the logs above: `memory: usage 5120kB, limit 5120kB, failcnt 69` shows it hit the limit,
`Killed process 16426 (java) total-vm:2438176kB, anon-rss:3200kB, file-rss:9440kB, shmem-rss:0kB` shows
that it decided to kill process **16426** which was our java process. There is a lot more information in the 
logs which can help identify the reason why the OOM killer killed your process, however in our case we know why - 
we violated the container memory limit. 

With a heap issue, if we hit an out of memory error with `Java Heap Space` as the cause, we know
immediately that the cause is the heap and we are either allocating too much, or we need to increase 
the heap (actually identifying the underlying cause of this overallocation in the code is another issue...). 
When the OOM killer kills our process, it's not so straightforward - it could be direct buffers,
unconstrained heap memory areas (_Metaspace_, Code cache etc...) or even another process within the container.
There is quite a bit to cover when investigating. On that note, I'll finish this post.

# Conclusion
There is quite a lot more that could be said about heap/non-heap memory in the JVM, docker and the oom-killer - 
but I want to keep this initial post short, it's just meant to be a basic intro to JVM memory usage. Hopefully, 
if you took anything away from this post, it's that there is much more to think about than just the heap when 
using the JVM, especially in memory bound containers.

In the next post I'll dive 
into non-heap memory in more depth, create and run a real containerized application, and go through some of the 
pitfalls I've come across when running JVM's within containerized environments.

[^1]: See [NMT details](https://docs.oracle.com/javase/8/docs/technotes/guides/troubleshoot/tooldescr022.html).
[^2]: This one I need to look up more in-depth, as I have not been able to find solid information on it.
[^3]: Arena Chunk seems to be related to malloc arenas, will definitely look into this in-depth.
[^4]: 1 MiB = 1024 KiB = 1048576 bytes. Why use MiB? Because MB is ambiguous and can mean 1000 KB or 1024 KB, whereas MiB is always 1024 KiB.
[^5]: See [this great answer](https://stackoverflow.com/questions/7880784/what-is-rss-and-vsz-in-linux-memory-management#answer-21049737) for a description of RSS.
[^6]: A detailed description of them can be found [here](https://docs.oracle.com/javase/8/docs/technotes/guides/troubleshoot/memleaks002.html).
[^7]: The `docker` documentation on this subject is excellent - see [resource constraints](https://docs.docker.com/config/containers/resource_constraints/).
[^8]: See [docker memory swappiness](https://github.com/docker/docker-ce/blob/17.12/components/engine/pkg/sysinfo/sysinfo_linux.go#L90).
