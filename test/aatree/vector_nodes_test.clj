(ns aatree.vector-nodes-test
  (:require [clojure.test :refer :all]
            [aatree.nodes :refer :all]
            [aatree.vector-nodes :refer :all]))

(def v0 (create-empty-vector-node))
(pnodev v0 "v0")
