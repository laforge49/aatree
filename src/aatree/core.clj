(ns aatree.core
  (:import (aatree AAMap AAVector)
           (java.util Comparator)))

(defn create-aamap
  ([] (new AAMap))
  ([^Comparator comparator] (new AAMap comparator)))

(defn create-vector []
  (new AAVector))
