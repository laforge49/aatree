(ns aatree.standard-vector-examples
  (:require [aatree.core :refer :all]))

(set! *warn-on-reflection* true)

(def opts (standard-opts))

(def bv1 (conj (new-vector opts) 1 2 3))
(println bv1); -> [1 2 3]

(def s1 (seq bv1))
(println s1); -> (1 2 3)
(println (count s1)); -> 3

(def s2 (next s1))
(println s2); -> (2 3)
(println (count s2)); -> 2

(println (rseq bv1)); -> (3 2 1)

