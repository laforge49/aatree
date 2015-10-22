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
        aamap (calf-get-map opts)
        _ (is (= aamap {}))
        _ (calf-update (fn [aamap opts]
                         (assoc aamap :fun "Clojure"))
                       opts)
        aamap (calf-get-map opts)
        _ (is (= aamap {:fun "Clojure"}))
        _ (is (= (calf-transaction-count opts) 3))
        _ (calf-close opts)])

  (let [opts (calf-open (File. "calf-test.calf") 10000)
        _ (is (= (calf-transaction-count opts) 3))
        aamap (calf-get-map opts)
        _ (is (= aamap {:fun "Clojure"}))
        _ (calf-close opts)]))
