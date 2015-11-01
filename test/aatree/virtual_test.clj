(ns aatree.virtual-test
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]
            [aatree.yearling :refer :all])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(deftest virtual
  (.delete (File. "virtual-test.yearling"))

  (let [opts (yearling-open (File. "virtual-test.yearling")
                            {:send-update-timeout 300})]
    (reduce (fn [_ j]
              (db-update (fn [aamap opts]
                           (reduce (fn [m i]
                                     (assoc m (+ i (* j 100000)) 1))
                                   aamap
                                   (range 1000)))
                         opts)
              )
            0
            (range 100))
    (db-close opts))

  (Thread/sleep 200))
