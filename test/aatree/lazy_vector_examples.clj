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
