(ns aatree.core
  (:require [aatree.nodes :refer :all])
  (:require [aatree.lazy-nodes :refer :all])
  (:import (aatree AAMap AAVector)
           (aatree.nodes FlexVector)
           (java.util Comparator)))

(set! *warn-on-reflection* true)

(defn create-aamap
  ([] (new AAMap (create-empty-node)))
  ([^Comparator comparator] (new AAMap (create-empty-node) comparator)))

(defn create-aavector []
  (new AAVector (create-empty-node)))

(defn addn [^FlexVector vec ndx val]
  (.addNode vec ndx val))

(defn dropn [vec & args]
  (reduce (fn [^FlexVector v i] (.dropNode v i)) vec args))

(defn create-lazy-aamap
  ([] (new AAMap (create-lazy-empty-node)))
  ([^Comparator comparator] (new AAMap (create-lazy-empty-node) comparator)))

(defn create-lazy-aavector []
  (new AAVector (create-lazy-empty-node)))
