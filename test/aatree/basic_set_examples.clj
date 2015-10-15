(ns aatree.basic-set-examples
  (:require [aatree.core :refer :all]))

(set! *warn-on-reflection* true)

(def opts (basic-opts))

(def bs1 (conj (new-set opts) :dog :cat :rabbit))
(println bs1); -> #{:cat :dog :rabbit}

(println (nth bs1 1)); -> :dog

(println (rseq bs1)); -> (:rabbit :dog :cat)
