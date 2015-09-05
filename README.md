# aatree

A Clojure library for AA Trees.

[AA Trees](https://en.wikipedia.org/wiki/AA_tree) 
are simpler than red-black trees,
which makes them easier to modify and extend.
But the performance is about the same.

Use (aatree.core.create-aamap) or
(aatree.core.create-aamap Comparator) to create a sorted 
map.

Validation of aamap has been done using 
[collection-check](https://github.com/ztellman/collection-check).

Compiled AOT with Clojure 1.7.0.
