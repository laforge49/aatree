(ns aatree.lazy-vector-examples
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)))

(println (lazy-byte-length emptyLazyAAVector)); -> 1

(def lv1 (conj emptyLazyAAVector 1 2 3))
(println lv1); -> [1 2 3]

(def lv1-len (lazy-byte-length lv1))
(println lv1-len); -> 61

(def bb (ByteBuffer/allocate lv1-len))
(lazy-write lv1 bb)
(.flip bb)
(def lv2 (load-aavector bb))
(println lv2); -> [1 2 3]
