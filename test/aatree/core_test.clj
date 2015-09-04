(ns aatree.core-test
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]
            [collection-check :refer :all]
            [clojure.test.check.generators :as gen]))

(def x (create-aamap))
(println (.entryAt x 1))
(println (.containsKey x 1))
(def x1 (.assoc x 1 1000))
(println (.entryAt x1 1))
(println (.containsKey x1 1))
(def x2 (.without x1 1))
(println (.entryAt x2 1))
(println (.containsKey x2 1))
(println (.seq x1))
(def x3 (.empty x1))
(println (.entryAt x3 1))
(println (.containsKey x3 1))
(println (.seq x3))
(def x12 (.assoc x1 2 1002))
(println (.entryAt x12 1))
(println (.containsKey x12 1))
(println (.seq x12))
(println (.rseq x12))
(println (.count x12))
(def x123 (.assoc x12 3 1003))
(println x123)
(println (.seqFrom x123 2 true))
(println (.seqFrom x123 2 false))

(def gen-element
  (gen/tuple gen/int))

(assert-map-like 100 (create-aamap)
                 gen-element gen-element)
