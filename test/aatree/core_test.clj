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

(deftest map-tests
  (assert-map-like 100
                   (create-aamap)
                   gen-element gen-element
                   {:base (sorted-map) :ordered? true}))

(println)
(def t1 (create-aamap))
(def t2 (assoc t1 3 -3))
(println t2)
(def t3 (assoc t2 3 2))
(println t3)
(def t4 (assoc t3 -2 2))
(println t4 (count t4))

(def t8 (-> {}
            (assoc (clojure.core/with-meta [0] {:foo 0}) (clojure.core/with-meta [0] {:foo 0}))
            (assoc (clojure.core/with-meta [0] {:foo 0}) (clojure.core/with-meta [0] {:foo nil}))))
(println t8)
(println (meta (first (seq (keys t8)))))
(println (meta (first (seq (vals t8)))))

(def t9 (-> (create-aamap)
            (assoc (clojure.core/with-meta [0] {:foo 0}) (clojure.core/with-meta [0] {:foo 0}))
            (assoc (clojure.core/with-meta [0] {:foo 0}) (clojure.core/with-meta [0] {:foo nil}))))
(println t9)
(println (meta (first (seq (keys t9)))))
(println (meta (first (seq (vals t9)))))

(println)
(println (nth x123 -1 nil))
(println (nth x123 0 nil))
(println (nth x123 1 nil))
(println (nth x123 2 nil))
(println (nth x123 3 nil))

(println)
(def y (create-aavector))
(println (count y))
(def y1 (conj y 1001))
(println (count y1))
(def y12 (addn y1 1 1002))
(println (count y12))
(def y012 (addn y12 0 1000))
(println (count y012))
(println y012)
(println (pop y012))
(println (pop (pop y012)))
(println (pop (pop (pop y012))))
(println (addn (conj y 0) 0 0))
(println (dropn y012 0))
(println (dropn y012 1))
(println (dropn y012 1 1))
(println (dropn y012 3))

(deftest vec-tests
  (assert-vector-like 100 (create-aavector) gen-element))
