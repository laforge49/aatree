(ns aatree.virtual-benchmark
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]
            [aatree.yearling :refer :all])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(comment

  (deftest virtual
    (.delete (File. "virtual-benchmark.yearling"))

    (let [opts (yearling-open (File. "virtual-benchmark.yearling"))
          ;        mxi 100000
          mxi 3
          mxj 5
          mxk 2]
      (time
        (reduce
          (fn [_ k]
            (reduce
              (fn [_ j]
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
          (range mxk))) ; -> "Elapsed time: 39369.950654 msecs"
      (println "count" (count (db-get-sorted-map opts)))
      (time (reduce
              (fn [_ i] (get (db-get-sorted-map opts) i))
              nil
              (range (count (db-get-sorted-map opts))))) ; -> "Elapsed time: 8404.581527 msecs"
      (db-close opts))

    (Thread/sleep 200))
  )