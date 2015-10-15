(ns aatree.standard-set-examples
  (:require [aatree.core :refer :all]))

(set! *warn-on-reflection* true)

(def opts (standard-opts))

(def bs1 (conj (new-set opts) :dog :cat :rabbit))
(println bs1); -> #{:cat :dog :rabbit}

(println (rseq bs1)); -> (:rabbit :dog :cat)

