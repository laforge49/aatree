(ns aatree.virtual-test
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]
            [aatree.yearling :refer :all])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(deftest virtual
  (.delete (File. "virtual-test.yearling"))

  (let [opts (yearling-open (File. "virtual-test.yearling") 200 600
                            {:send-update-timeout 300})]
    (db-update (fn [aamap opts]
                 (assoc aamap :a 1))
               opts)
    (db-update (fn [aamap opts]
                 (assoc aamap :b 1))
               opts)
    (println "All is well?")
    (db-close opts))

  (Thread/sleep 200))
