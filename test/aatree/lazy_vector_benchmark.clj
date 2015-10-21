(ns aatree.lazy-vector-benchmark
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)))

(set! *warn-on-reflection* true)

;(def vector-size 1000000)
(def vector-size 1)
;(def updates 1000)
(def updates 1)

(println)
(def t0 (System/currentTimeMillis))
(def lazy-vector (reduce conj (new-vector (lazy-opts)) (range vector-size)))
(def t1 (System/currentTimeMillis))
(def micr-0 (* 1000. (- t1 t0)))
(println "Time to build a lazy vector of size" vector-size "=" micr-0 "microseconds")
(println "Time per entry:" (/ micr-0 vector-size) "microseconds")

(defn upd [v i]
  (let [v1 (assoc v i (- i))
        bb (ByteBuffer/allocate (byte-length v1))]
    (put-aa bb v1)
    (.flip bb)
    (load-vector bb (lazy-opts))))

(println)
(def t0 (System/currentTimeMillis))
(def lazy-vector (reduce upd lazy-vector (range updates)))
(def t1 (System/currentTimeMillis))
(def micr-0 (* 1000. (- t1 t0)))
(println "Time to deserialize/update/reserialize " updates "times =" micr-0 "microseconds")
(println "Time per complete update:" (/ micr-0 updates) "microseconds")

(println)