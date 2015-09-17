(ns aatree.core
  (:require [aatree.nodes :refer :all])
  (:require [aatree.lazy-nodes :refer :all])
  (:import (aatree AAMap AAVector)
           (aatree.nodes FlexVector)
           (java.util Comparator)))

(set! *warn-on-reflection* true)

(def aamap (new AAMap (create-empty-node)))

(defn create-aamap
  ([] aamap)
  ([^Comparator comparator] (new AAMap (create-empty-node) comparator)))

(def aavector (new AAVector (create-empty-node)))

(defn create-aavector []
  aavector)

(defn addn [^FlexVector vec ndx val]
  (.addNode vec ndx val))

(defn dropn [vec & args]
  (reduce (fn [^FlexVector v i] (.dropNode v i)) vec args))

(def lazy-aamap (new AAMap lazy-node))

(defn create-lazy-aamap
  ([] lazy-aamap)
  ([^Comparator comparator] (new AAMap lazy-node comparator))
  ([^Comparator comparator fregistry] (new AAMap (create-lazy-empty-node fregistry) comparator)))

(def lazy-aavector (new AAVector lazy-node))

(defn create-lazy-aavector
  ([] lazy-aavector)
  ([fregistry] (new AAVector (create-lazy-empty-node fregistry)))
  )
