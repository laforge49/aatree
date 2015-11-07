(ns aatree.virtual-benchmark
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]
            [aatree.yearling :refer :all])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(deftest virtual
  (.delete (File. "virtual-benchmark.yearling"))

  (let [opts (yearling-open (File. "virtual-benchmark.yearling"))
        mxi 100
        mxj 10]
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
        (range mxj)))
    (println (count (db-get-sorted-map opts)))
    (db-close opts))

  (Thread/sleep 200))
