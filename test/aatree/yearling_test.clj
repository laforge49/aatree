(ns aatree.yearling-test
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]
            [aatree.yearling :refer :all]
            [aatree.closer-trait :refer :all])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(deftest yearling
  (.delete (File. "yearling-test.yearling"))

  (let [yearling {:max-db-size   100000
                  :db-block-size 10000}
        yearling (yearling-open yearling (File. "yearling-test.yearling"))
        _ (is (= (db-transaction-count yearling) 2))
        aamap (db-get-sorted-map yearling)
        _ (is (= aamap {}))
        _ (db-update
            yearling
            (fn [yearling aamap]
              (assoc aamap :block (db-allocate yearling))))
        aamap (db-get-sorted-map yearling)
        _ (is (= aamap {:block 20000}))
        _ (is (= (db-transaction-count yearling) 3))
        _ (is (= (db-allocated yearling) 3))
        _ (is (= (count (db-release-pending yearling)) 0))
        _ (db-update
            yearling
            (fn [yearling aamap]
              (println "new node id" ((:db-new-node-id yearling)))
              (db-release yearling (:block aamap))
              (dissoc aamap :block)))
        _ (is (= (db-transaction-count yearling) 4))
        aamap (db-get-sorted-map yearling)
        _ (is (= aamap {}))
        _ (is (= (db-allocated yearling) 3))
        _ (is (= (count (db-release-pending yearling)) 1))
        _ (do-close yearling)])

  (let [yearling {:db-pending-count 99
                  :max-db-size      100000
                  :db-block-size    10000}
        yearling (yearling-open yearling (File. "yearling-test.yearling"))
        _ (is (= (db-transaction-count yearling) 4))
        aamap (db-get-sorted-map yearling)
        _ (is (= aamap {}))
        _ (is (= (db-allocated yearling) 3))
        _ (is (= (count (db-release-pending yearling)) 1))
        _ (db-update
            yearling
            (fn [yearling aamap]
              (println "new node id" ((:db-new-node-id yearling)))
              (db-process-pending yearling 0 1)
              aamap))
        _ (is (= (db-transaction-count yearling) 5))
        aamap (db-get-sorted-map yearling)
        _ (is (= aamap {}))
        _ (is (= (db-allocated yearling) 2))
        _ (is (= (count (db-release-pending yearling)) 0))
        _ (do-close yearling)])

  (Thread/sleep 200))
