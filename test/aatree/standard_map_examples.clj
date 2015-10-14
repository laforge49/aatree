(ns aatree.standard-map-examples
  (:require [aatree.core :refer :all]))

(set! *warn-on-reflection* true)

(def opts (standard-opts))

(def bm1 (conj (new-aamap opts) {:dog "Jack" :cat "Sammy" :rabbit "Henry"}))
(println bm1); -> {:cat Sammy, :dog Jack, :rabbit Henry}

(println (rseq bm1)); -> ([:rabbit Henry] [:dog Jack] [:cat Sammy])

