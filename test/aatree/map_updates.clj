(ns aatree.map-updates
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)))

(def map-size 1000000)
(def updates 1000)

(defn bld [m i]
  (conj m [i i]))

(println)
(def t0 (System/currentTimeMillis))
(def lazy-map (reduce bld emptyLazyAAMap (range map-size)))
(def t1 (System/currentTimeMillis))
(def micr-0 (* 1000. (- t1 t0)))
(println "Time to build a map of size" map-size "=" micr-0 "microseconds")
(println "Time per entry:" (/ micr-0 map-size) "microseconds")

(defn upd [m i]
  (let [m1 (assoc m i (- i))
        bb (ByteBuffer/allocate (lazy-byte-length m1))]
    (lazy-write m1 bb)
    (.flip bb)
    (load-aamap bb)))

(println)
(def t0 (System/currentTimeMillis))
(def lazy-m (reduce upd lazy-map (range updates)))
(def t1 (System/currentTimeMillis))
(def micr-0 (* 1000. (- t1 t0)))
(println "Time to deserialize/update/reserialize " updates "times =" micr-0 "microseconds")
(println "Time per complete update:" (/ micr-0 updates) "microseconds")

(println)