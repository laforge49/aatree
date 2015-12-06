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
        app-map (db-get-in yearling [:app-map])
        _ (is (= (get-transaction-count yearling) 2))
        _ (is (= app-map nil))
        _ (is (= (db-allocated yearling) 2))
        _ (db-update
            yearling
            (fn [db]
              (update-assoc-in! db [:app-map :block] (db-allocate db))))
        block (db-get-in yearling [:app-map :block])
        _ (is (= (get-transaction-count yearling) 3))
        _ (is (= block 20000))
        _ (is (= (db-allocated yearling) 3))
        _ (is (= (count (db-release-pending yearling)) 0))
        _ (db-update
            yearling
            (fn [db]
              (println "new node id" (db-new-node-id db))
              (db-release db block)
              (update-dissoc-in! db [:app-map :block])))
        app-map (db-get-in yearling [:app-map])
        _ (is (= (get-transaction-count yearling) 4))
        _ (is (= app-map nil))
        _ (is (= (db-allocated yearling) 3))
        _ (is (= (count (db-release-pending yearling)) 1))
        _ (close-components yearling)])

  (let [yearling {:db-pending-count 99
                  :max-db-size      100000
                  :db-block-size    10000}
        yearling (yearling-open yearling (File. "yearling-test.yearling"))
        app-map (db-get-in yearling [:app-map])
        _ (is (= (get-transaction-count yearling) 4))
        _ (is (= app-map nil))
        _ (is (= (db-allocated yearling) 3))
        _ (is (= (count (db-release-pending yearling)) 1))
        _ (db-update
            yearling
            (fn [db]
              (println "new node id" (db-new-node-id db))
              (db-process-pending db 0 1)))
        app-map (db-get-in yearling [:app-map])
        _ (is (= (get-transaction-count yearling) 5))
        _ (is (= app-map nil))
        _ (is (= (db-allocated yearling) 2))
        _ (is (= (count (db-release-pending yearling)) 0))
        _ (close-components yearling)])

  (Thread/sleep 200))
