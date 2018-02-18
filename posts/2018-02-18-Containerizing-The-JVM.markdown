---
title: "Containerizing The JVM: Memory Usage Overview"
tags: jvm,container
---

# Introduction
For the past few years where I work has been running JVM
applications on [Cloud Foundry](https://www.cloudfoundry.org/) 
and have come across many issues in relation to memory when running JVM's within memory 
bounded containers. 

Cloud foundry runs applications inside [garden containers](https://www.cloudfoundry.org/blog/cloud-foundry-containers-difference-warden-docker-garden/). 
These containers have memory limits set on application creation, they use cgroups
just as docker does and if the sum of the memory allocated to each process within the container
exceeds the limit, the oom-killer will kick in and kill the while container.  
The main issue we've seen with JVM applications is Non-Heap Memory. Tracking and tuning the 
heap is trivial in comparison to tracking and tuning non-heap memory usage in the JVM - by non-heap
I dont just mean Metaspace, but also direct buffers, code cache, stack sizes, and so on.

I've been wanting to do a series of posts
on the topic of JVM's in containers for a while, I finally put the time aside to start!

I'm going to keep this post short and just give an overview of the heap and non-heap 
memory regions of the JVM, with some small introduction to both. This post assumes some
basic knowledge of Java, the JVM and docker. It also assumes you have docker and openjdk 8 installed. 

# Containerizing a java app
To start I'm going to keep things super simple. Let's build a program
which prints "Hello world!" and waits forever:

```java
public class HelloWorld {
  public static void main(String[] args) throws Exception {
    System.out.println("Hello world!");
    System.in.read();
  }
}
```

Now, a simple Dockerfile:

```dockerfile
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
First, get into the container and get the JVM processes PID.

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
First off, the single letter class names. These are all documented under [Class.getName()](https://docs.oracle.com/javase/6/docs/api/java/lang/Class.html#getName()).

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

Note the above are all _native_ types, not objects. If you look back to the `jmap` output, the 
first few instances all have `[` prefixing them - e.g. `[I`. `[` denotes an 1 dimensional 
array of the type proceeding it - `[I` denotes and array of `int` e.g. `new int[3]`. 
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
That's a quick overview of one way to look at what's loaded in the heap. I mentioned other memory 
regions in the JVM earlier, what are these?

# Heap and Non-Heap memory
Again, I'm going to keep this simple. The JVM can be divided into many different memory segments, 
if we take a high level view first we have two segments - memory used for objects on the heap and 
non-heap memory. 

If we zoom in, the heap has different areas within 
which we can talk about, depending on what we want to discuss - there is the Eden space, where most
new objects are initially created, the Survivor space, where objects go if they survive an Eden space
garbage collection (GC) and the Old Generation which contains objects that have lived in 
Survivor Space for a while. The contains objects that have been initialize - e.g. 
`List<String> s = new ArrayList<String>();` will create an arraylist object on the heap.

In the previous section I ran through what classes are loaded into the heap for our HelloWorld program,
so what about non-heap memory?

## Non-Heap Memory 
If you have ever written a non-trivial java application with jdk8 you have probably heard of metaspace.
This is an example of non-heap memory. It's where the JVM will store classes, static variables, methods, 
classloaders and other metadata. But there are many other non-heap memory regions the JVM will use.
Let's list them! 

To do so, first we need to enable native memory tracking in our java app:

```dockerfile
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
In another terminal, exec into the container and get a baseline with `jcmd`'s `VM.native_memory` command:

```bash
$ docker exec --privileged -ti hello-jvm bash
root@aa5ae77e1305:/ $ jcmd 
33 sun.tools.jcmd.JCmd
7 HelloWorld
root@aa5ae77e1305:/ $ jcmd 7 VM.native_memory baseline
7:
Baseline succeeded
```
Finally, to get a summary of used memory:

```bash
root@aa5ae77e1305:/# jcmd 7 VM.native_memory summary
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

  * **Java Heap** : is the used heap memory. 
  * **Class** : is the metaspace region we spoke about.
  * **Thread** : is the space taken up by threads on this JVM's.
  * **Code** : is the code cache - this is used by the JIT to cache compiled code.
  * **GC** : space used by the garbage collector.
  * **Compiler** : space used by the JIT when generating code.
  * **Symbols** : this is for symbols, by which I believe field names, method signatures fall under. [^2]
  * **Native Memory Tracking** : memory used by the native memory tracker itself.
  * **Arena Chunk** : not entirely sure what this gets used for. [^3]

# Practical Issues
Ok, so why should I care about any of the above? Let's create an app that eats a tonne of memory.

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
This will create a `Vector` which contains 400 byte arrays of size 1 MiB[^4], so this will create a heap of ~400MiB in size.
It will then sleep for 10 seconds so we can get the memory usage easily while it runs.
Let's constrain the heap to 450MiB and run this locally we can see the actual memory usage of the process of the process. 
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
In total, the JVM process needs about 500 MiB to run. What happens if we set the heap to a size lower than it needs?

```bash
$ java -Xms400M -Xmx400M MemEater
Exception in thread "main" java.lang.OutOfMemoryError: Java heap space
	at MemEater.main(MemEater.java:7)
```
If you have used java (or any JVM language) before, you have more than likely come across this. It means that the 
JVM ran out of heap space to allocate objects. There are quite a few other types of `OutOfMemoryError` the JVM can 
throw in certain situations [^6], but I won't go into more detail right now.

Ok, so now we know what happens if the JVM does not have enough heap space, what about the case where you are running 
in a container, have plenty of heap space, but you hit the overall memory limit?

The simplest way to reproduce this is to package up our `MemEater` program into a docker image and run it with 
less memory than it needs.

```dockerfile
```

# Conclusion
There is quite a lot more that could be said about heap and non-heap memory, but I want to keep this 
initial post short, it's just meant to be a basic intro to JVM memory usage. Hopefully, if you took anything
away from this post, it's that there is much more to think about than just the heap when using the JVM - 
especially in memory bound containers.

In the next post I'll dive 
into non-heap memory in more depth, create and run a real containerized application, and go through the 
pitfalls of tuning within containerized environment.

[^1]: See [NMT details](https://docs.oracle.com/javase/8/docs/technotes/guides/troubleshoot/tooldescr022.html).
[^2]: This one I need to look up more in-depth, as I have not been able to find solid information on it.
[^3]: Arena Chunk seems to be related to malloc arenas, will definitely look into this in-depth.
[^4]: 1 MiB = 1024 KiB = 1048576 bytes. Why use MiB? Because MB is ambiguous and can mean 1000 KB or 1024 KB, whereas MiB is always 1024 KiB.
[^5]: See [this great answer](https://stackoverflow.com/questions/7880784/what-is-rss-and-vsz-in-linux-memory-management#answer-21049737) for a description of RSS.
[^6]: A detailed description of them can be found [here](https://docs.oracle.com/javase/8/docs/technotes/guides/troubleshoot/memleaks002.html).
