(ns aatree.lazy-sorted-map-examples
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)))

(set! *warn-on-reflection* true)

(def opts (lazy-opts))

(def empty-set (new-sorted-set opts))
(def empty-map (new-sorted-map opts))
(def empty-vec (new-vector opts))

(println (byte-length empty-map)); -> 1

(def lm1 (conj empty-map {:dog "Jack" :cat "Sammy" :rabbit "Henry"}))
(println lm1); -> {:cat Sammy, :dog Jack, :rabbit Henry}

(def lm1-len (byte-length lm1))
(println lm1-len); -> 143

(def ^ByteBuffer bb (ByteBuffer/allocate lm1-len))
(put-bytebuffer lm1 bb)
(.flip bb)
(def lm2 (load-sorted-map bb opts))
(println lm2); -> {:cat Sammy, :dog Jack, :rabbit Henry}

(def lv1 (conj empty-vec 1 2 3))
(def lm3 (conj lm2 [:vct lv1]))
(println lm3); -> {:cat Sammy, :dog Jack, :vct [1 2 3], :rabbit Henry}

(def lm3-len (byte-length lm3))
(println lm3-len); -> 230

(def ^ByteBuffer bb (ByteBuffer/allocate lm3-len))
(put-bytebuffer lm3 bb)
(.flip bb)
(def lm4 (load-sorted-map bb opts))
(println lm4); -> {:cat Sammy, :dog Jack, :vct [1 2 3], :rabbit Henry}

(def lm5 (conj empty-map {"b" :bandana "h" :hat}))
(def lm6 (conj lm4 [:map lm5]))
(println lm6); -> {:cat Sammy, :dog Jack, :vct [1 2 3], :map {b :bandana, h :hat}, :rabbit Henry}

(def lm6-len (byte-length lm6))
(println lm6-len); -> 341

(def ^ByteBuffer bb (ByteBuffer/allocate lm6-len))
(put-bytebuffer lm6 bb)
(.flip bb)
(def lm7 (load-sorted-map bb opts))
(println lm7); -> {:cat Sammy, :dog Jack, :vct [1 2 3], :map {b :bandana, h :hat}, :rabbit Henry}

(def ls1 (conj empty-set "a" "c" "b"))
(def lm8 (conj lm7 [:set ls1]))
(println lm8); -> {:cat Sammy, :dog Jack, :map {b :bandana, h :hat}, :rabbit Henry, :set #{a b c}, :vct [1 2 3]}

(def lm8-len (byte-length lm8))
(println lm8-len); -> 440

(def ^ByteBuffer bb (ByteBuffer/allocate lm8-len))
(put-bytebuffer lm8 bb)
(.flip bb)
(def lm9 (load-sorted-map bb opts))
(println lm9); -> {:cat Sammy, :dog Jack, :map {b :bandana, h :hat}, :rabbit Henry, :set #{a b c}, :vct [1 2 3]}
