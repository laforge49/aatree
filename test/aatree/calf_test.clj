(ns aatree.calf-test
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]
            [aatree.calf :refer :all]
            [aatree.closer-trait :refer :all])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(deftest calf
  (.delete (File. "calf-test.calf"))

  (let [calf (calf-open (File. "calf-test.calf") 10000)
        db-state (db-get-state calf)
        _ (is (= (:transaction-count db-state) 2))
        _ (is (= (get-in db-state [:uber-map :app-map]) nil))
        _ (db-update calf
                     (fn [db db-state]
                       (assoc-in db-state [:uber-map :app-map :fun] "Clojure")))
        db-state (db-get-state calf)
        _ (is (= (get-in db-state [:uber-map :app-map :fun]) "Clojure"))
        _ (is (= (:transaction-count db-state) 3))
        _ (close-components calf)])

  (let [calf (calf-open (File. "calf-test.calf") 10000)
        db-state (db-get-state calf)
        _ (is (= (:transaction-count db-state) 3))
        _ (is (= (get-in db-state [:uber-map :app-map :fun]) "Clojure"))
        _ (close-components calf)]))
