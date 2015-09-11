# aatree

A Clojure library for AA Trees.

[AA Trees](https://en.wikipedia.org/wiki/AA_tree) 
are simpler than red-black trees,
which makes them easier to modify and extend.
But the performance is about the same.

Use (aatree.core.create-aamap) or
(aatree.core.create-aamap Comparator) to create a sorted 
map.
Written entirely in Clojure, aamap extends clojure.lang.APersistentMap
and implements clojure.lang.IObj, clojure.lang.Reversible,
clojure.lang.Sorted, clojure.lang.Counted and clojure.lang.Indexed.

Use (aatree.core.create-aavector) to create a vector.
Written entirely in Clojure, aavector extends clojure.lang.APersistentVector,
clojure.lang.IObj and aatree.vector-nodes.flex-vector.

Two new functions, addn and dropn, are provided for adding/dropping values
at any location in aavector. These are reasonably fast, as aavector is
implemented as a tree.

Validation of aamap and aavector has been done using 
[collection-check](https://github.com/ztellman/collection-check).

Compiled AOT with Clojure 1.7.0. Reflection has been avoided through the
use of *warn-on-reflection*.
