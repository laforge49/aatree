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

;new in 0.3.2

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

(def lm5 (conj emptyLazyAAMap {"b" :bandana "h" :hat}))
(def lm6 (conj lm4 [:map lm5]))
(println lm6); -> {:cat Sammy, :dog Jack, :list [1 2 3], :map {b :bandana, h :hat}, :rabbit Henry}

(def lm6-len (lazy-byte-length lm6))
(println lm6-len); -> 343

(def ^ByteBuffer bb (ByteBuffer/allocate lm6-len))
(lazy-write lm6 bb)
(.flip bb)
(def lm7 (load-aamap bb))
(println lm7); -> {:cat Sammy, :dog Jack, :list [1 2 3], :map {b :bandana, h :hat}, :rabbit Henry}
