(ns aatree.core-test
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]))

(def x (create-aamap))
(println (.entryAt x 1))
(println (.containsKey x 1))
