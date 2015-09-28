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

Time to build a lazy map of size 1000000 = 2.4381E7 microseconds.

Time per entry: 24.381 microseconds.

Time to deserialize/update/reserialize  1000 times = 3.1047E7 microseconds.

Time per complete update: 31047.0 microseconds.

(Times will vary depending on the host system used to run the benchmarks.)

## Lazy Vector Benchmark

Time to build a lazy vector of size 1000000 = 2.6543E7 microseconds.

Time per entry: 26.543 microseconds.

Time to deserialize/update/reserialize  1000 times = 1.668E7 microseconds.

Time per complete update: 16680.0 microseconds.

(Times will vary depending on the host system used to run the benchmarks.)
