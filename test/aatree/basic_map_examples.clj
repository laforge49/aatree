(ns aatree.basic-map-examples
  (:require [aatree.core :refer :all]))

(def bm1 (conj emptyAAMap {:dog "Jack" :cat "Sammy" :rabbit "Henry"}))
(println bm1); -> {:cat Sammy, :dog Jack, :rabbit Henry}

(println (nth bm1 1)); [:dog Jack]
