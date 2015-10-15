(ns aatree.vector-updates
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)))

(set! *warn-on-reflection* true)

(def vector-size 1000000)
(def updates 1000)

(println)
(def t0 (System/currentTimeMillis))
(def lazy-vector (reduce conj (new-vector (lazy-opts)) (range vector-size)))
(def t1 (System/currentTimeMillis))
(def micr-0 (* 1000. (- t1 t0)))
(println "Time to build a vector of size" vector-size "=" micr-0 "microseconds")
(println "Time per entry:" (/ micr-0 vector-size) "microseconds")

(defn upd [v i]
  (let [v1 (assoc v i (- i))
        bb (ByteBuffer/allocate (lazy-byte-length v1))]
    (lazy-write v1 bb)
    (.flip bb)
    (load-aavector bb)))

(println)
(def t0 (System/currentTimeMillis))
(def lazy-vector (reduce upd lazy-vector (range updates)))
(def t1 (System/currentTimeMillis))
(def micr-0 (* 1000. (- t1 t0)))
(println "Time to deserialize/update/reserialize " updates "times =" micr-0 "microseconds")
(println "Time per complete update:" (/ micr-0 updates) "microseconds")

(println)