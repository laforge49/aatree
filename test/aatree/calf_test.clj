(ns aatree.calf-test
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]
            [aatree.calf :refer :all])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(deftest calf
  (.delete (File. "calf-test.calf"))

  (let [opts (calf-open (File. "calf-test.calf") 10000)
        _ (is (= (calf-transaction-count opts) 2))
        data (calf-get opts)
        _ (is (= data {}))
        data (assoc data :fun "Clojure")
        _ (is (= data {:fun "Clojure"}))
        _ (calf-put data opts)
        data (calf-get opts)
        _ (is (= data {:fun "Clojure"}))
        _ (is (= (calf-transaction-count opts) 3))
        _ (calf-close opts)])

  (let [opts (calf-open (File. "calf-test.calf") 10000)
        _ (is (= (calf-transaction-count opts) 3))
        data (calf-get opts)
        _ (is (= data {:fun "Clojure"}))
        _ (calf-close opts)]))
