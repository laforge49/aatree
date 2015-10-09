(ns aatree.lazy-map-examples
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)))

(set! *warn-on-reflection* true)

(println (lazy-byte-length emptyLazyAAMap)); -> 1

(def lm1 (conj emptyLazyAAMap {:dog "Jack" :cat "Sammy" :rabbit "Henry"}))
(println lm1); -> {:cat Sammy, :dog Jack, :rabbit Henry}

(def lm1-len (lazy-byte-length lm1))
(println lm1-len); -> 143

(def ^ByteBuffer bb (ByteBuffer/allocate lm1-len))
(lazy-write lm1 bb)
(.flip bb)
(def lm2 (load-aamap bb))
(println lm2); -> {:cat Sammy, :dog Jack, :rabbit Henry}

(def lv1 (conj emptyLazyAAVector 1 2 3))
(def lm3 (conj lm2 [:list lv1]))
(println lm3); -> {:cat Sammy, :dog Jack, :list [1 2 3], :rabbit Henry}

(def lm3-len (lazy-byte-length lm3))
(println lm3-len); -> 232

(def ^ByteBuffer bb (ByteBuffer/allocate lm3-len))
(lazy-write lm3 bb)
(.flip bb)
(def lm4 (load-aamap bb))
(println lm4); -> {:cat Sammy, :dog Jack, :list [1 2 3], :rabbit Henry}
