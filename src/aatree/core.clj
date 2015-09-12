(ns aatree.core
  (:require [aatree.nodes :refer :all])
  (:import (aatree AAMap AAVector)
           (java.util Comparator)))

(set! *warn-on-reflection* true)

(defn create-aamap
  ([] (new AAMap (create-empty-node)))
  ([^Comparator comparator] (create-empty-node) (new AAMap comparator)))

(defn create-aavector []
  (new AAVector (create-empty-node)))

(defn addn [^AAVector vec ndx val]
  (.addNode vec ndx val))

(defn dropn [aavector & args]
  (reduce (fn [^AAVector v i] (.dropNode v i)) aavector args))
