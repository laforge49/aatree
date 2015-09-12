(ns aatree.nodes-test
  (:require [clojure.test :refer :all]
            [aatree.nodes :refer :all]))

(def v0 (create-empty-node))
(pnodev v0 "v0")

(def v1 (vector-add v0 1001 0))
(pnodev v1 "v1")

(def v01 (vector-add v1 1000 0))
(pnodev v01 "v01")

(def v012 (vector-add v01 1002 2))
(pnodev v012 "v012")

(pnodev (deln v012 0) "v012 - 0")

(pnodev (deln v012 1) "v012 - 1")

(pnodev (deln v012 2) "v012 - 2")
