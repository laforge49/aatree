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
