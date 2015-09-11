(ns aatree.core
  (:import (aatree AAMap AAVector)
           (java.util Comparator)))

(set! *warn-on-reflection* true)

(defn create-aamap
  ([] (new AAMap))
  ([^Comparator comparator] (new AAMap comparator)))

(defn create-aavector []
  (new AAVector))

(defn addn [^AAVector aavector ndx val]
  (.addn aavector ndx val))

(defn dropn [^AAVector aavector & args]
  (reduce (fn [v i] (.dropn v i)) aavector args))
