(ns aatree.lazy-vector-examples
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)))

(set! *warn-on-reflection* true)

(def opts (lazy-opts))

(def empty-set (new-sorted-set opts))
(def empty-map (new-sorted-map opts))
(def empty-vec (new-vector opts))

(println (byte-length empty-vec)); -> 1

(def lv1 (conj empty-vec 1 2 3))
(println lv1); -> [1 2 3]

(def lv1-len (byte-length lv1))
(println lv1-len); -> 61

(def ^ByteBuffer bb (ByteBuffer/allocate lv1-len))
(put-aa bb lv1)
(.flip bb)
(def lv2 (load-vector bb opts))
(println lv2); -> [1 2 3]

(def lv3 (conj empty-vec lv1))
(println lv3); -> [[1 2 3]]

(def lv3-len (byte-length lv3))
(println lv3-len); -> 76

(def ^ByteBuffer bb (ByteBuffer/allocate lv3-len))
(put-aa bb lv3)
(.flip bb)
(def lv4 (load-vector bb opts))
(println lv4); -> [[1 2 3]]

(def lv5 (lv4 0))
(println lv5); -> [1 2 3]
(println (class lv5)); -> aatree.AAVector

(def lm1 (conj empty-map {:dog "Jack" :cat "Sammy" :rabbit "Henry"}))
(def lv6 (conj lv4 lm1))
(println lv6); -> [[1 2 3] {:cat Sammy, :dog Jack, :rabbit Henry}]

(def lv6-len (byte-length lv6))
(println lv6-len); -> 233

(def ^ByteBuffer bb (ByteBuffer/allocate lv6-len))
(put-aa bb lv6)
(.flip bb)
(def lv7 (load-vector bb opts))
(println lv7); -> [[1 2 3] {:cat Sammy, :dog Jack, :rabbit Henry}]

(def lm2 (lv7 1))
(println lm2); -> {:cat Sammy, :dog Jack, :rabbit Henry}
(println (class lm2)); -> aatree.AAMap

(def ls1 (conj empty-set :dog :cat :rabbit))
(def lv8 (conj lv7 ls1))
(println lv8); -> [[1 2 3] {:cat Sammy, :dog Jack, :rabbit Henry}
             ;     #{:cat :dog :rabbit}]

(def lv8-len (byte-length lv8))
(println lv8-len); -> 332

(def ^ByteBuffer bb (ByteBuffer/allocate lv8-len))
(put-aa bb lv8)
(.flip bb)
(def lv9 (load-vector bb opts))
(println lv9); -> [[1 2 3] {:cat Sammy, :dog Jack, :rabbit Henry}
             ;     #{:cat :dog :rabbit}]

(def ls2 (lv9 2))
(println ls2); -> #{:cat :dog :rabbit}
(println (class ls2)); -> aatree.AASet
