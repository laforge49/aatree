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

Validation has been done using 
[collection-check](https://github.com/ztellman/collection-check).
Compiled AOT with Clojure 1.7.0. Reflection has been avoided through the
use of warn-on-reflection.

##[Releases](https://github.com/laforge49/aatree/releases)
[![Clojars Project](http://clojars.org/aatree/latest-version.svg)](http://clojars.org/aatree)

## Wiki

[API](https://github.com/laforge49/aatree/wiki/API)

[Lazy Benchmarks](https://github.com/laforge49/aatree/wiki/Lazy-Benchmarks)

[Immutable Value, Mutable Form](https://github.com/laforge49/aatree/wiki/Immutable-Value,-Mutable-Form)

[Towards Greater Code Reuse](https://github.com/laforge49/aatree/wiki/Towards-Greater-Code-Reuse)

### Durable Applications--File Load and Save

It is not often that you need lazy deserialization when doing a file load. 
Rather, we are using file load and save here to illustrate how to use the lazy 
structures of aatree. 

1. [File Load and Save](https://github.com/laforge49/aatree/wiki/File-Load-and-Save)
1. [Using Adler32](https://github.com/laforge49/aatree/wiki/Using-Adler32)
1. [A 256-bit Checksum](https://github.com/laforge49/aatree/wiki/A-256-Bit-Checksum)

### Write Me a Database

It is easy enough to code up 
[A Small Database](https://github.com/laforge49/aatree/wiki/A-Small-Database)
using lazy aatree structures.
The catch is that the contents of the database must fit in memory.

### Adding Disk Space Management

[Yearling](https://github.com/laforge49/aatree/wiki/Yearling)
takes the next step in providing
[Disk Space Management](https://github.com/laforge49/aatree/wiki/Disk-Space-Management).
But like Calf, Yearling remains a small Copy-On-Write \[COW\] database where
everything must fit in memory.

Later we will look at how to implement larger databases. But there will be a lot more code involved.
So it is best to take things one step at a time.
