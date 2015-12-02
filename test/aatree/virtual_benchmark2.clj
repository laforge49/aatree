(ns aatree.virtual-benchmark2
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]
            [aatree.yearling2 :refer :all]
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
                  (fn [db]
                    (let [aamap (update-get-in db [:uber-map :app-map])
                          bbmap (reduce (fn [m i]
                                          (assoc m (+ i (* mxi j) (* mxi mxj k)) 1))
                                        aamap
                                        (range mxi))]
                      (update-assoc-in db [:uber-map :app-map] bbmap)))))
              nil
              (range mxj)))
          nil
          (range mxk)))                                     ; -> "Elapsed time: 39369.950654 msecs"
      (println "count" (count (db-get-state yearling [:uber-map :app-map])))
      (let [app-map (db-get-state yearling [:uber-map :app-map])]
        (time (reduce
                (fn [_ i] (get app-map i))
                nil
                (range (count (db-get-state yearling [:uber-map :app-map])))))) ; -> "Elapsed time: 8404.581527 msecs"
      (close-components yearling))

    (Thread/sleep 200))
