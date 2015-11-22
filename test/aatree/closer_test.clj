(ns aatree.closer-test
  (:require [clojure.test :refer :all]
            [aatree.closer :refer :all]))

(set! *warn-on-reflection* true)

(deftest closer
  (defn close-a [opts] (println "close a"))
  (defn close-b [opts] (println "close b"))

  (is (nil? (get-close close-a {})))

  (let [opts (on-close close-a {})]
    (is (not (nil? (get-close close-a opts))))
    (do-close opts)
    (is (nil? (get-close close-a opts)))
    (do-close opts))

  (Thread/sleep 100))
