(ns aatree.closer-test
  (:require [aatree.closer :refer :all]))

(set! *warn-on-reflection* true)

(defn close-a [opts] (println "  close a"))
(defn close-b [opts] (println "  close b"))
(defn close-c [opts] (println "  close c"))

(let [opts (on-close close-a {})
      opts (on-close close-b opts)
      opts (on-close close-c opts)]
  (println "first close")
  (do-close opts)
  (println "second close")
  (do-close opts))
