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
;        mxi 3
        mxj 5
        mxk 2]
    (time
      (reduce
        (fn [_ k]
          (println "k" k)
          (reduce
            (fn [_ j]
              (println "j" j)
              (db-update (fn [aamap opts]
                           (let [bbmap (reduce (fn [m i]
                                                 (assoc m (+ i (* mxi j) (* mxi mxj k)) 1))
                                               aamap
                                               (range mxi)
                                               )]
                             bbmap))
                         opts))
            nil
            (range mxj)))
        nil
        (range mxk))); -> "Elapsed time: 35351.58856 msecs"
    (println "count" (count (db-get-sorted-map opts)))
    (time (reduce
      (fn [_ i] (get (db-get-sorted-map opts) i))
      nil
      (range (count (db-get-sorted-map opts))))); -> "Elapsed time: 9459.646228 msecs"
    (db-close opts))

  (Thread/sleep 200))
