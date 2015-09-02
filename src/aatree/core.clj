(ns aatree.core
  (:import (aatree AAMap)
           (java.util Comparator)))

(defn create-aamap
  ([] (new AAMap))
  ([^Comparator comparator] (new AAMap comparator)))
