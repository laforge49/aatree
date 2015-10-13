(ns aatree.lazy-vector-examples
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)))

(set! *warn-on-reflection* true)

(println (lazy-byte-length emptyLazyAAVector)); -> 1

(def lv1 (conj emptyLazyAAVector 1 2 3))
(println lv1); -> [1 2 3]

(def lv1-len (lazy-byte-length lv1))
(println lv1-len); -> 61

(def ^ByteBuffer bb (ByteBuffer/allocate lv1-len))
(lazy-write lv1 bb)
(.flip bb)
(def lv2 (load-aavector bb))
(println lv2); -> [1 2 3]

;new in 0.3.2

(def lv3 (conj emptyLazyAAVector lv1))
(println lv3); -> [[1 2 3]]

(def lv3-len (lazy-byte-length lv3))
(println lv3-len); -> 76

(def ^ByteBuffer bb (ByteBuffer/allocate lv3-len))
(lazy-write lv3 bb)
(.flip bb)
(def lv4 (load-aavector bb))
(println lv4); -> [[1 2 3]]

(def lv5 (lv4 0))
(println lv5); -> [1 2 3]
(println (class lv5)); -> aatree.AAVector

(def lm1 (conj emptyLazyAAMap {:dog "Jack" :cat "Sammy" :rabbit "Henry"}))
(def lv6 (conj lv4 lm1))
(println lv6); -> [[1 2 3] {:cat Sammy, :dog Jack, :rabbit Henry}]

(def lv6-len (lazy-byte-length lv6))
(println lv6-len); -> 233

(def ^ByteBuffer bb (ByteBuffer/allocate lv6-len))
(lazy-write lv6 bb)
(.flip bb)
(def lv7 (load-aavector bb))
(println lv7); -> [[1 2 3] {:cat Sammy, :dog Jack, :rabbit Henry}]

(def lm2 (lv7 1))
(println lm2); -> {:cat Sammy, :dog Jack, :rabbit Henry}
(println (class lm2)); -> aatree.AAMap

;new in 0.3.3

(def ls1 (conj emptyLazyAASet :dog :cat :rabbit))
(def lv8 (conj lv7 ls1))
(println lv8); -> [[1 2 3] {:cat Sammy, :dog Jack, :rabbit Henry}
             ;     #{:cat :dog :rabbit}]

(def lv8-len (lazy-byte-length lv8))
(println lv8-len); -> 332

(def ^ByteBuffer bb (ByteBuffer/allocate lv8-len))
(lazy-write lv8 bb)
(.flip bb)
(def lv9 (load-aavector bb))
(println lv9); -> [[1 2 3] {:cat Sammy, :dog Jack, :rabbit Henry}
             ;     #{:cat :dog :rabbit}]

(def ls2 (lv9 2))
(println ls2); -> #{:cat :dog :rabbit}
(println (class ls2)); -> aatree.AASet
