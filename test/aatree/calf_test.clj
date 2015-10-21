(ns aatree.calf-test
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]
            [aatree.calf :refer :all])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(deftest calf
  (def ^File calf-test (File. "calf-test.calf"))
  (.delete calf-test)

  (def opts (calf-open calf-test 10000))
  (is (= (calf-transaction-count opts) 2))
  (def data (calf-get opts))
  (is (= data {}))
  (def data (assoc data :fun "Clojure"))
  (is (= data {:fun "Clojure"}))
  (calf-write data opts)
  (def data (calf-get opts))
  (is (= data {:fun "Clojure"}))
  (is (= (calf-transaction-count opts) 3))
  (db-close opts)

  (def opts (calf-open calf-test 10000))
  (is (= (calf-transaction-count opts) 3))
  (def data (calf-get opts))
  (is (= data {:fun "Clojure"}))
  (db-close opts)

  )
