(ns aatree.closer-trait-test
  (:require [aatree.closer-trait :refer :all]))

(set! *warn-on-reflection* true)

(close-components {})

(defn close-a [this] (println "  close a"))
(defn close-b [this] (println "  close b"))
(defn close-c [this] (println "  close c"))

(let [this (open-component {} "a" close-a)
      this (open-component this "b" close-b)
      this (open-component this "c" close-c)]
  (println "first close")
  (close-components this)
  (println "second close")
  (close-components this))
