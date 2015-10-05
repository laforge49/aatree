(ns aatree.lazy-map-examples
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)))

(println (lazy-byte-length emptyLazyAAMap)); -> 1

(def lm1 (conj emptyLazyAAMap {:dog "Jack" :cat "Sammy" :rabbit "Henry"}))
(println lm1); -> {:cat Sammy, :dog Jack, :rabbit Henry}

(def lm1-len (lazy-byte-length lm1))
(println lm1-len); -> 143

(def bb (ByteBuffer/allocate lm1-len))
(lazy-write lm1 bb)
(.flip bb)
(def lv2 (load-aamap bb))
(println lv2); -> {:cat Sammy, :dog Jack, :rabbit Henry}
