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
        app-map (db-get-in calf [:app-map])
        _ (is (= (get-transaction-count calf) 2))
        _ (is (= app-map nil))
        _ (db-update calf
                     (fn [db]
                       (update-assoc-in!
                         db
                         [:app-map :fun]
                         "Clojure")))
        fun (db-get-in calf [:app-map :fun])
        _ (is (= fun "Clojure"))
        _ (is (= (get-transaction-count calf) 3))
        _ (close-components calf)])

  (let [calf (calf-open (File. "calf-test.calf") 10000)
        fun (db-get-in calf [:app-map :fun])
        _ (is (= (get-transaction-count calf) 3))
        _ (is (= fun "Clojure"))
        _ (close-components calf)]))
