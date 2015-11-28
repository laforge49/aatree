# aatree

A Clojure library for AA Trees.

[AA Trees](https://en.wikipedia.org/wiki/AA_tree) 
are simpler than red-black trees,
which makes them easier to modify and extend.
But the performance is about the same.

In addition to implementing complete replacements for vector, sorted-set 
and sorted-map, an extension is
provided for lazy deserialization/reserialization.
This can be used to minimize the time to deserialize, update and reserialize
a large block of data. Because only a small portion of a data block
needs to be processed, processing is ridiculously fast when compared to
the processing time needed if the entire data block is deserialized / reserialized,
as is typical of applications requiring significant durable data.

Another extension is provided to support virtual data structures.
Structures no longer need to fit in memory, as only the parts of interest need to be
loaded.

Validation has been done using 
[collection-check](https://github.com/ztellman/collection-check).
Compiled AOT with Clojure 1.7.0. Reflection has been avoided through the
use of warn-on-reflection.

## [Releases](https://github.com/laforge49/aatree/releases)
[![Clojars Project](http://clojars.org/aatree/latest-version.svg)](http://clojars.org/aatree)

## Resources: [![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/laforge49/aatree?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge), [google group](https://groups.google.com/forum/?hl=en#!forum/agilewikidevelopers)

## Log4J 2

Logging dependencies:
```
                 [org.clojure/tools.logging "0.3.1"]
                 [org.apache.logging.log4j/log4j-core "2.4.1"]
                 [org.apache.logging.log4j/log4j-slf4j-impl "2.4.1"]
```

Sample log4j2-test.xml file (add to classpath):
```
<?xml version="1.0" encoding="UTF-8"?>
<Configuration status="INFO">
    <Appenders>
        <Console name="Console" target="SYSTEM_OUT">
            <PatternLayout pattern="%d{HH:mm:ss.SSS} [%t] %-5level %logger{36} - %msg%n"/>
        </Console>
    </Appenders>
    <Loggers>
        <Root level="info">
            <AppenderRef ref="Console"/>
        </Root>
    </Loggers>
</Configuration>
```

For testing purposes, the above log4j2-test.xml file was 
put in the dev-resources folder to keep it from being added to the jar file.

## Wiki

[Virtual Structures, easy to use but almost too simple to explain](https://github.com/laforge49/aatree/wiki/Virtual-Structures,-easy-to-use-but-almost-too-simple-to-explain)

[API](https://github.com/laforge49/aatree/wiki/API)

[Benchmarks](https://github.com/laforge49/aatree/wiki/Benchmarks)

### Blog

* [Ultralight Components](https://github.com/laforge49/aatree/wiki/Ultralight-Components)
* [Clojure Objects](https://github.com/laforge49/aatree/wiki/Clojure-Objects)
* [Towards Greater Code Reuse](https://github.com/laforge49/aatree/wiki/Towards-Greater-Code-Reuse)
* [Immutable Value, Mutable Form](https://github.com/laforge49/aatree/wiki/Immutable-Value,-Mutable-Form)

### Durable Applications--File Load and Save

It is not often that you need lazy deserialization when doing a file load. 
Rather, we are using file load and save here to illustrate how to use the lazy 
structures of aatree. 

1. [File Load and Save](https://github.com/laforge49/aatree/wiki/File-Load-and-Save)
1. [Using Adler32](https://github.com/laforge49/aatree/wiki/Using-Adler32)
1. [A 256-bit Checksum](https://github.com/laforge49/aatree/wiki/A-256-Bit-Checksum)

### Write Me a Database

It is easy enough to code up a small database, like
[Calf](https://github.com/laforge49/aatree/wiki/Calf),
using lazy aatree structures.
The catch is that the contents of the database must fit in memory.

### Virtual Data Structures

The [Yearling](https://github.com/laforge49/aatree/wiki/Yearling) database
supports virtual data structures, which allows for structures 
that are larger than will fit in memory.
[Disk Space Management](https://github.com/laforge49/aatree/wiki/Disk-Space-Management)
is also part of Yearling.

### The Heifer Database

Next up will be a database with a transaction log: 
[Heifer](https://github.com/laforge49/aatree/issues/28).
