(ns aatree.virtual-test
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]
            [aatree.yearling :refer :all])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(deftest virtual
  (.delete (File. "virtual-test.yearling"))

  (let [opts (yearling-open (File. "virtual-test.yearling") 700 (* 4 700)
                            {:send-update-timeout 300})]
    (db-update (fn [aamap opts]
                 (assoc aamap :a 1))
               opts)
    (db-update (fn [aamap opts]
                 (assoc aamap :b 2))
               opts)
    (db-update (fn [aamap opts]
                 (assoc aamap :c 3))
               opts)
    (db-update (fn [aamap opts]
                 (assoc aamap :d 4))
               opts)
    (db-update (fn [aamap opts]
                 (assoc aamap :e 5))
               opts)
    (db-update (fn [aamap opts]
                 (assoc aamap :f 6))
               opts)
;    (println (db-allocated opts))
;    (println (count (db-release-pending opts)))
    (db-close opts))

  (Thread/sleep 200))
