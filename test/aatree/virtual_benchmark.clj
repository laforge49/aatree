(ns aatree.virtual-benchmark
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]
            [aatree.yearling :refer :all])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(deftest virtual
  (.delete (File. "virtual-benchmark.yearling"))

  (let [opts (yearling-open (File. "virtual-benchmark.yearling"))
        mxi 100000
;        mxi 1
        mxj 100]
    (time
      (reduce
        (fn [_ j]
          (println j)
          (db-update (fn [aamap opts]
                       (let [bbmap (reduce (fn [m i]
                                             (assoc m (+ i (* mxi j)) 1))
                                           aamap
                                           (range mxi)
                                           )]
                         bbmap))
                     opts)
          )
        nil
        (range mxj))); -> "Elapsed time: 38830.891007 msecs"
    (println (count (db-get-sorted-map opts)))
    (time (reduce
      (fn [_ i] (get (db-get-sorted-map opts) i))
      nil
      (range (count (db-get-sorted-map opts))))); -> "Elapsed time: 9329.289874 msecs"
    (db-close opts))

  (Thread/sleep 200))
