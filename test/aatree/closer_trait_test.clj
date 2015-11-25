(ns aatree.closer-trait-test
  (:require [aatree.closer-trait :refer :all]))

(set! *warn-on-reflection* true)

(do-close {})

(defn close-a [this] (println "  close a"))
(defn close-b [this] (println "  close b"))
(defn close-c [this] (println "  close c"))

(let [this (on-close {} close-a "a")
      this (on-close this close-b "b")
      this (on-close this close-c "c")]
  (println "first close")
  (do-close this)
  (println "second close")
  (do-close this))
