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
        _ (is (= (db-transaction-count calf) 2))
        aamap (db-get-sorted-map calf)
        _ (is (= aamap {}))
        _ (db-update calf
                     (fn [calf aamap]
                       (assoc aamap :fun "Clojure")))
        aamap (db-get-sorted-map calf)
        _ (is (= aamap {:fun "Clojure"}))
        _ (is (= (db-transaction-count calf) 3))
        _ (do-close calf)])

  (let [calf (calf-open (File. "calf-test.calf") 10000)
        _ (is (= (db-transaction-count calf) 3))
        aamap (db-get-sorted-map calf)
        _ (is (= aamap {:fun "Clojure"}))
        _ (do-close calf)]))
