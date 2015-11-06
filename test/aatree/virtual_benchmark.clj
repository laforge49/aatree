(ns aatree.virtual-benchmark
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]
            [aatree.yearling :refer :all])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(deftest virtual
  (.delete (File. "virtual-benchmark.yearling"))

  (let [opts {:send-update-timeout 300}
        opts (yearling-open (File. "virtual-benchmark.yearling") opts)]
    (time
      (db-update (fn [aamap opts]
                   (let [bbmap (reduce (fn [m i]
                                         (assoc m i 1))
                                       aamap
                                       (range 100)
                                       )]
                     (println bbmap)
                     (Thread/sleep 200)
                     bbmap))
                 opts)
      )
    (println "dump:" (db-get-sorted-map opts))
    (comment
      (time
        (db-update (fn [aamap opts]
                     (println 111111)
                     (let [bbmap (reduce (fn [m i]
                                           (assoc m i 1))
                                         aamap
                                         (range 3 6)
                                         )]
                       (println bbmap)
                       (Thread/sleep 200)
                       bbmap))
                   opts)
        )
      )
    (Thread/sleep 200)
    (db-close opts))

  (Thread/sleep 200))
