(ns aatree.core
  (:import (aatree AAMap)))

(defn create-aamap
  ([] (new AAMap))
  ([comparator] (new AAMap comparator)))
