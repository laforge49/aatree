(ns aatree.core-test
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]))

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
