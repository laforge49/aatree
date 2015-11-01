(ns aatree.virtual-test
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]
            [aatree.yearling :refer :all])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(deftest virtual
  (.delete (File. "virtual-test.yearling"))

  (let [opts (yearling-open (File. "virtual-test.yearling") 400 100000
                            {:send-update-timeout 300})]
    (db-update (fn [aamap opts]
                 (assoc aamap :a 1))
               opts)
    (println 1 (db-allocated opts) (count (db-release-pending opts)))
    (db-update (fn [aamap opts]
                 (assoc aamap :b 2))
               opts)
    (println 2 (db-allocated opts) (count (db-release-pending opts)))
    (db-update (fn [aamap opts]
                 (assoc aamap :c 3))
               opts)
    (println 3 (db-allocated opts) (count (db-release-pending opts)))
    (db-update (fn [aamap opts]
                 (assoc aamap :d 4))
               opts)
    (println 4 (db-allocated opts) (count (db-release-pending opts)))
    (db-update (fn [aamap opts]
                 (assoc aamap :e 5))
               opts)
    (println 5 (db-allocated opts) (count (db-release-pending opts)))
    (db-update (fn [aamap opts]
                 (assoc aamap :f 6))
               opts)
    (println 6 (db-allocated opts) (count (db-release-pending opts)))
    (db-update (fn [aamap opts]
                 (assoc aamap :f 6))
               opts)
    (println 7 (db-allocated opts) (count (db-release-pending opts)))
    (db-update (fn [aamap opts]
                 (assoc aamap :f 6))
               opts)
    (println 8 (db-allocated opts) (count (db-release-pending opts)))
    (db-update (fn [aamap opts]
                 (assoc aamap :f 6))
               opts)
    (println 9 (db-allocated opts) (count (db-release-pending opts)))
    (db-update (fn [aamap opts]
                 (assoc aamap :f 6))
               opts)
    (println 10 (db-allocated opts) (count (db-release-pending opts)))
    (db-close opts))

  (Thread/sleep 200))
