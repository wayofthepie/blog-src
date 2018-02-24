---
title: "Containerizing The JVM: Understanding Memory Mapping"
tags: jvm,container
---

# Introduction
The last post focused on the high level organization of memory within the JVM, 
both heap and non-heap. This post will be more practical, we'll build a basic
spring boot app and run through some pitfalls when bounding memory withing 
containers with a focus on how memory is mapped in the OS.

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

# Spring Boot memory usage
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
a deeper look with Native Memory Tracking (NMT) - don't forget to kill the existing application instance,
you can use `kill -9 %1` if it is the first job in the jobs list:

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
linux memory management, specifically the `mmap` system call.

## The JVM and mmap
