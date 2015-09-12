(ns aatree.lazy-nodes
  (:require [aatree.nodes :refer :all]))

(set! *warn-on-reflection* true)

(defn create-lazy-empty-node []
  (create-empty-node))
