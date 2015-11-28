(ns aatree.virtual-benchmark
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]
            [aatree.yearling :refer :all]
            [aatree.closer-trait :refer :all])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

  (deftest virtual
    (.delete (File. "virtual-benchmark.yearling"))

    (let [yearling (yearling-open (File. "virtual-benchmark.yearling"))
          ;        mxi 100000
          mxi 3
          mxj 5
          mxk 2]
      (time
        (reduce
          (fn [_ k]
            (reduce
              (fn [_ j]
                (db-update
                  yearling
                  (fn [yearling aamap]
                    (let [bbmap (reduce (fn [m i]
                                          (assoc m (+ i (* mxi j) (* mxi mxj k)) 1))
                                        aamap
                                        (range mxi))]
                      bbmap))))
              nil
              (range mxj)))
          nil
          (range mxk)))                                     ; -> "Elapsed time: 39369.950654 msecs"
      (println "count" (count (db-get-sorted-map yearling)))
      (time (reduce
              (fn [_ i] (get (db-get-sorted-map yearling) i))
              nil
              (range (count (db-get-sorted-map yearling))))) ; -> "Elapsed time: 8404.581527 msecs"
      (do-close yearling))

    (Thread/sleep 200))
