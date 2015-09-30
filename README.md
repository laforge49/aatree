# aatree

A Clojure library for AA Trees.

[AA Trees](https://en.wikipedia.org/wiki/AA_tree) 
are simpler than red-black trees,
which makes them easier to modify and extend.
But the performance is about the same.

In addition to implementing complete replacements for vector and map, an extension is
provided for lazy deserialization/reserialization.
These can be used to minimize the time to deserialize, update and reserialize
a large block of data. Because only a small portion of a data block
needs to be processed, processing is ridiculously fast when compared to
the processing time needed if the entire data block is deserialized / reserialized,
as is typical of applications requiring significant durable data.

Validation has been done using 
[collection-check](https://github.com/ztellman/collection-check).
Compiled AOT with Clojure 1.7.0. Reflection has been avoided through the
use of warn-on-reflection.

## Lazy Map Benchmark

Time to build a lazy map of size 1,000,000 = 24 seconds.

Time per entry: 24 microseconds.

Time to deserialize/update/reserialize  1,000 times = 31 seconds.

Time per complete update: 31 milliseconds.

(Times will vary depending on the host system used to run the benchmarks.)

## Lazy Vector Benchmark

Time to build a lazy vector of size 1,000,000 = 27 seconds.

Time per entry: 27 seconds.

Time to deserialize/update/reserialize  1,000 times = 17 seconds.

Time per complete update: 17 milliseconds.

(Times will vary depending on the host system used to run the benchmarks.)

##[Releases](https://github.com/laforge49/aatree/releases)

