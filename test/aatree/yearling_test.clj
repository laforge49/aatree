(ns aatree.yearling-test
  (:require [clojure.test :refer :all]
            [medley.core :refer :all]
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
        db-state (db-get-state yearling)
        _ (is (= (:transaction-count db-state) 2))
        _ (is (= (get-in db-state [:uber-map :app-map]) nil))
        _ (is (= (db-allocated yearling) 2))
        _ (db-update
            yearling
            (fn [db db-state]
              (assoc-in db-state [:uber-map :app-map :block] (db-allocate db))))
        db-state (db-get-state yearling)
        _ (is (= (:transaction-count db-state) 3))
        block (get-in db-state [:uber-map :app-map :block])
        _ (is (= block 20000))
        _ (is (= (db-allocated yearling) 3))
        _ (is (= (count (db-release-pending yearling)) 0))
        _ (db-update
            yearling
            (fn [db db-state]
              (println "new node id" ((:db-new-node-id db)))
              (db-release db block)
              (dissoc-in db-state [:uber-map :app-map :block])))
        db-state (db-get-state yearling)
        _ (is (= (:transaction-count db-state) 4))
        _ (is (= (get-in db-state [:uber-map :app-map]) nil))
        _ (is (= (db-allocated yearling) 3))
        _ (is (= (count (db-release-pending yearling)) 1))
        _ (close-components yearling)])

  (let [yearling {:db-pending-count 99
                  :max-db-size      100000
                  :db-block-size    10000}
        yearling (yearling-open yearling (File. "yearling-test.yearling"))
        db-state (db-get-state yearling)
        _ (is (= (:transaction-count db-state) 4))
        _ (is (= (get-in db-state [:uber-map :app-map]) nil))
        _ (is (= (db-allocated yearling) 3))
        _ (is (= (count (db-release-pending yearling)) 1))
        _ (db-update
            yearling
            (fn [db db-state]
              (println "new node id" ((:db-new-node-id db)))
              (db-process-pending db 0 1)
              db-state))
        db-state (db-get-state yearling)
        _ (is (= (:transaction-count db-state) 5))
        _ (is (= (get-in db-state [:uber-map :app-map]) nil))
        _ (is (= (db-allocated yearling) 2))
        _ (is (= (count (db-release-pending yearling)) 0))
        _ (close-components yearling)])

  (Thread/sleep 200))
