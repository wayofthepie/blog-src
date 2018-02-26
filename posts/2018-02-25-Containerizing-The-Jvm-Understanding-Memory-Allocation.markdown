---
title: "Containerizing The JVM: Understanding Memory Allocation"
tags: jvm,container
---

# Introduction
The last post focused on the high level organization of memory within the JVM, 
both heap and non-heap. This post will be more practical, we'll build a basic
spring boot app and run through some pitfalls when bounding memory within 
containers with a focus on how memory is mapped in the OS. I could have just
used another Hello World program, but I think using a Spring Boot app
is more representative of a real application.

# Basic Spring Boot application
You can generate an application from the [Spring Initializr](https://start.spring.io/) website.
Lets create one with the following details:

  * Generate a __Gradle project__ with __Java__ and Spring Boot __2__ (spring boot version is 2.0 RC2 at the time of writing)
  * Group : com.bootmem
  * Artifact : spring-boot-mem

Now generate the project, and unzip it into a directory.

## Building and running
To build:

```bash
$ ./gradlew build
Build cache is an incubating feature.

> Task :test 
2018-02-24 14:28:25.328  INFO 17569 --- [       Thread-5] s.c.a.AnnotationConfigApplicationContext : Closing org.springframework.context.annotation.AnnotationConfigApplicationContext@e879a5f: startup date [Sat Feb 24 14:28:24 GMT 2018]; root of context hierarchy


BUILD SUCCESSFUL in 2s
5 actionable tasks: 3 executed, 2 up-to-date
```

To run, after building:
```bash
$ java -jar build/libs/spring-boot-mem-0.0.1-SNAPSHOT.jar 

  .   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
 \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
  '  |____| .__|_| |_|_| |_\__, | / / / /
 =========|_|==============|___/=/_/_/_/
 :: Spring Boot ::            (v2.0.0.RC2)

2018-02-24 14:29:48.764  INFO 17847 --- [           main] c.b.s.SpringBootMemApplication           : Starting SpringBootMemApplication on chaospie-Precision-5520 with PID 17847 (/home/chaospie/repos/spring-boot-mem/build/libs/spring-boot-mem-0.0.1-SNAPSHOT.jar started by chaospie in /home/chaospie/repos/spring-boot-mem)
2018-02-24 14:29:48.767  INFO 17847 --- [           main] c.b.s.SpringBootMemApplication           : No active profile set, falling back to default profiles: default
2018-02-24 14:29:48.832  INFO 17847 --- [           main] s.c.a.AnnotationConfigApplicationContext : Refreshing org.springframework.context.annotation.AnnotationConfigApplicationContext@6c629d6e: startup date [Sat Feb 24 14:29:48 GMT 2018]; root of context hierarchy
2018-02-24 14:29:49.248  INFO 17847 --- [           main] o.s.j.e.a.AnnotationMBeanExporter        : Registering beans for JMX exposure on startup
2018-02-24 14:29:49.256  INFO 17847 --- [           main] c.b.s.SpringBootMemApplication           : Started SpringBootMemApplication in 0.797 seconds (JVM running for 1.123)
2018-02-24 14:29:49.273  INFO 17847 --- [       Thread-2] s.c.a.AnnotationConfigApplicationContext : Closing org.springframework.context.annotation.AnnotationConfigApplicationContext@6c629d6e: startup date [Sat Feb 24 14:29:48 GMT 2018]; root of context hierarchy
2018-02-24 14:29:49.278  INFO 17847 --- [       Thread-2] o.s.j.e.a.AnnotationMBeanExporter        : Unregistering JMX-exposed beans on shutdown
```

Notice it just exits. We need to add some logic, first modify `build.gradle` to pull in `org.springframework.boot:spring-boot-starter-web`
instead of `org.springframework.boot:spring-boot-starter`

```groovy
...
dependencies {
	compile('org.springframework.boot:spring-boot-starter-web')
	testCompile('org.springframework.boot:spring-boot-starter-test')
}
...
```

Rebuild with `./gradlew build` to pull in this dependency. We can now create a basic rest endpoint:

```java
// src/java/com/bootmem/springbootmem/HelloWorldController.java
package com.bootmem.springbootmem;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping("/helloworld")
public class HelloWorldController {
    @RequestMapping(method = RequestMethod.GET)
    public @ResponseBody String helloWorld() {
        return "Hello world!";
    }
}

```

Now, running again the app will stay listening on port 8080. A quick way to run the app is to use `./gradlew bootRun`, 
to test our endpoint:

```bash
$ curl http://localhost:8080/helloworld
Hello world!
```

In the rest of this post I will use `java -jar` directly to launch the application. 
That's the basic setup done. Now, time to dive in.

# Application memory usage
So how much memory does our application use, seeing as we have not given it any 
bounds:

```bash
$ nohup java -jar build/libs/spring-boot-mem-0.0.1-SNAPSHOT.jar &
[1] 22399
nohup: ignoring input and appending output to 'nohup.out'

$ ps aux | awk 'NR==1; /[s]pring-boot/'
USER       PID %CPU %MEM    VSZ   RSS TTY      STAT START   TIME COMMAND
chaospie 22399 26.2  1.7 8220248 289912 pts/11 Sl   15:01   0:10 java -jar build/libs/spring-boot-mem-0.0.1-SNAPSHOT.jar
```

So the process uses 289MB `RSS`. We know from the last post this is not the full story, `RSS`
only shows memory _currently_ resident in physical memory for this process. Lets take
a deeper look with Native Memory Tracking (NMT) (don't forget to kill the existing application instance,
you can use `kill -9 %1` if it is the first job in the jobs list):

```bash
$ nohup java -jar build/libs/spring-boot-mem-0.0.1-SNAPSHOT.jar &
[1] 22399
nohup: ignoring input and appending output to 'nohup.out'

$ ps aux | awk 'NR==1; /[s]pring-boot/'
USER       PID %CPU %MEM    VSZ   RSS TTY      STAT START   TIME COMMAND
chaospie 22675 49.0  1.8 8286812 299044 pts/11 Sl   15:05   0:10 java -jar -XX:NativeMemoryTracking=summary build/libs/spring-boot-mem-0.0.1-SNAPSHOT.jar
```
Slightly higher initial `RSS`, likely because NMT adds a bit of overhead. Now lets get the NMT
details:

```bash
$ jcmd $(jcmd | grep spring-boot | awk '{print $1}') VM.native_memory summary
22675:

Native Memory Tracking:

Total: reserved=5632502KB, committed=542902KB
-                 Java Heap (reserved=4069376KB, committed=273408KB)
                            (mmap: reserved=4069376KB, committed=273408KB) 
 
-                     Class (reserved=1089300KB, committed=43796KB)
                            (classes #6248)
                            (malloc=10004KB #7901) 
                            (mmap: reserved=1079296KB, committed=33792KB) 
 
-                    Thread (reserved=40262KB, committed=40262KB)
                            (thread #40)
                            (stack: reserved=40092KB, committed=40092KB)
                            (malloc=125KB #205) 
                            (arena=46KB #78)
 
-                      Code (reserved=252380KB, committed=16756KB)
                            (malloc=2780KB #3886) 
                            (mmap: reserved=249600KB, committed=13976KB) 
 
-                        GC (reserved=159065KB, committed=146561KB)
                            (malloc=10385KB #204) 
                            (mmap: reserved=148680KB, committed=136176KB) 
 
-                  Compiler (reserved=143KB, committed=143KB)
                            (malloc=12KB #138) 
                            (arena=131KB #3)
 
-                  Internal (reserved=10699KB, committed=10699KB)
                            (malloc=10667KB #8703) 
                            (mmap: reserved=32KB, committed=32KB) 
 
-                    Symbol (reserved=9769KB, committed=9769KB)
                            (malloc=6916KB #62121) 
                            (arena=2853KB #1)
 
-    Native Memory Tracking (reserved=1311KB, committed=1311KB)
                            (malloc=7KB #86) 
                            (tracking overhead=1303KB)
 
-               Arena Chunk (reserved=196KB, committed=196KB)
                            (malloc=196KB) 
 

```
See the total committed, 542902KB! This is about 240MB bigger than the reported `RSS`, 
why is there such a difference? To understand this, we're going to have to dive into
linux memory management and the syscalls that enable this.

## Memory allocation
For this you're going to need `strace` installed, to allow us to trace system calls 
within our JVM process. First, what happens when the JVM is initialized, what does it 
do to reserve memory?

```bash
$ strace -e trace=memory -f -o out java -jar build/libs/spring-boot-mem-0.0.1-SNAPSHOT.jar
# Ctrl+C to kill once the app has booted

$ head out
26792 brk(NULL)                         = 0x63d000
26792 mmap(NULL, 4096, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0) = 0x7fd41bca9000
26792 mmap(NULL, 2155416, PROT_READ|PROT_EXEC, MAP_PRIVATE|MAP_DENYWRITE, 3, 0) = 0x7fd41b876000
26792 mprotect(0x7fd41b883000, 2097152, PROT_NONE) = 0
26792 mmap(0x7fd41ba83000, 8192, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_FIXED|MAP_DENYWRITE, 3, 0xd000) = 0x7fd41ba83000
26792 mmap(NULL, 108033, PROT_READ, MAP_PRIVATE, 3, 0) = 0x7fd41bc8e000
26792 mmap(NULL, 3971488, PROT_READ|PROT_EXEC, MAP_PRIVATE|MAP_DENYWRITE, 3, 0) = 0x7fd41b4ac000
26792 mprotect(0x7fd41b66c000, 2097152, PROT_NONE) = 0
26792 mmap(0x7fd41b86c000, 24576, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_FIXED|MAP_DENYWRITE, 3, 0x1c0000) = 0x7fd41b86c000
26792 mmap(0x7fd41b872000, 14752, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_FIXED|MAP_ANONYMOUS, -1, 0) = 0x7fd41b872000
...
```
The flags passed to `strace` do the following:

  * **-e trace=memory** trace memory related system calls.
  * __-f__ print details about child processes, this includes threads.
  * __-o out__ write the information to a file called `out`.
  * The rest is the command we want to run `java -jar ...`.

Running this, and waiting for our app to be live, there will be thousands of memory related syscalls.
Going through them, or even the code that causes them to happen, is beyond this post, however
we can simulate this to get a better understanding. Before we do that, lets get the types of syscalls 
used when tracing for memory:

```bash
$ strace -c -e trace=memory -f -o out java -jar build/libs/spring-boot-mem-0.0.1-SNAPSHOT.jar
# Ctrl+C to kill once the app has booted

$ cat out 
% time     seconds  usecs/call     calls    errors syscall
------ ----------- ----------- --------- --------- ----------------
 71.00    0.004769           1      6603           mprotect
 24.49    0.001645           2       667           mmap
  4.09    0.000275           4        65           munmap
  0.25    0.000017           6         3           brk
  0.16    0.000011           0        28           madvise
------ ----------- ----------- --------- --------- ----------------
100.00    0.006717                  7366           total
```

  * _mprotect_ - change protection for this processes memory pages.
  * _mmap_ - create a mapping on the virtual address space [^vaddr] for this process.
  * _munmap_ - delete mappings in the specified address ranges.
  * _brk_ - changes the amount of memory available to the process.
  * _madvise_ - give advice to the kernel about the given address range.


## Mapping some memory
It's time to dive into some `C`! Lets create a program which maps 1GiB of memory.

```c
// allocate.c
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>

int main(void) {
    size_t mmapLen = 1073741824;
    char *ptr = (char*) mmap(
        NULL // address
        , mmapLen // length of the mapping
        , PROT_READ|PROT_WRITE // protection applied to the mapping
        , MAP_PRIVATE|MAP_ANONYMOUS // flags
        , -1 // file descriptor
        , 0 // offset
    );
    
    if(ptr == MAP_FAILED) {
        perror("mmap");
        exit(EXIT_FAILURE);
    }

    sleep(100);

    return 0;
}

```
Now compile and run - assuming your `C` compiler is `gcc`:

```bash
$ gcc allocate.c -o allocate
$ nohup ./allocate &

$ ps aux | awk 'NR==1; /\.\/[a]lloc/'
USER       PID %CPU %MEM    VSZ   RSS TTY      STAT START   TIME COMMAND
chaospie 24479  0.0  0.0 1052796  648 pts/21   S    11:10   0:00 ./allocate
```
Notice `RSS` is only 648 KiB but `VSZ` (the virtual address space size) is about 1GiB, 
which is much closer to what we expect. Why? To answer that, we first need to understand the `mmap` syscall.

### mmap
From `mmap`'s man pages:

> mmap()  creates a new mapping in the virtual address space of the calling process.

The signature for the `mmap` syscall is as follows

```c
void *mmap(void *addr, size_t length, int prot, int flags,
                  int fd, off_t offset);
```
Our call was `mmap(NULL, mmapLen , PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);`, what does this do exactly?
In order of args left to right:

  * _NULL_ - this lets the _kernel_ chose the address to create the mapping.
  * _mmapLen_ - we defined this as `size_t mmapLen = 1073741824;` which is 1GiB, this is the size of the mapping.
  * _PROT_READ | PROT_WRITE_ - this mapping is readable and writeable.
  * _MAP_PRIVATE | MAP_ANONYMOUS_ 
    * _MAP_PRIVATE_ - tells it to create a private _copy on write_ mapping, updates to 
    the mapping are not visible to other processes.
    * _MAP_ANONYMOUS_ - says the mapping is not backed by any file.
  * _-1_ - we are using _MAP_ANONYMOUS_ so have no file descriptor, therefore we pass -1 here.
  * _0_ - we are using _MAP_ANONYMOUS_ and have no file, so there is no offset.


### Populating RSS
There is a flag called `MAP_POPULATE` which will populate the mapped memory. If we update 
our allocation program as follows, we'll see the allocated memory reflected in `RSS` :

```c
// allocate.c
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>

int main(void) {
    size_t mmapLen = 1073741824;
    char *ptr = (char*) mmap(
        NULL 
        , mmapLen
        , PROT_READ|PROT_WRITE 
        , MAP_PRIVATE|MAP_ANONYMOUS|MAP_POPULATE 
        , -1 
        , 0 
    );

    if(ptr == MAP_FAILED) {
        perror("mmap");
        exit(EXIT_FAILURE);
    }

    sleep(100);

    return 0;
}
```
Compile and run:

```bash
$ gcc allocate.c -o allocate
$ nohup ./allocate &

$ ps aux | awk 'NR==1; /\.\/[a]lloc/'
USER       PID %CPU %MEM    VSZ   RSS TTY      STAT START   TIME COMMAND
chaospie 32082  1.6  6.4 1052796 1049224 pts/21 S   12:51   0:00 ./allocate
```
We now see 1049224 KiB in use in `RSS` which is about 1GiB - just a few hundred KiB over what we allocated
with `mmap` (which was `1073741824 / 1024` = 1048576 KiB, a difference of 648 KiB which accounts for
other parts of our program). 

This shows that a process can allocate memory and only have it resident when it is actually used/needed. 
However it doesn't answer the more specific question that took us down this path - why is there a
difference between committed memory reported by the _JVM_ and `RSS`?

# 

[^vaddr]: See [Process Address Space](https://www.kernel.org/doc/gorman/html/understand/understand007.html).
