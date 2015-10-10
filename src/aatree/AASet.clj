(ns aatree.AASet
  (:gen-class
    :main false
    :extends clojure.lang.APersistentSet
    :implements [clojure.lang.IObj
                 clojure.lang.Reversible
                 clojure.lang.Sorted
                 clojure.lang.Counted
                 clojure.lang.Indexed
                 aatree.nodes.INoded]
    :constructors {[aatree.nodes.INode clojure.lang.IPersistentMap]
                   []
                   [aatree.nodes.INode clojure.lang.IPersistentMap clojure.lang.IPersistentMap]
                   []}
    :init init
    :state state)
  (:require [aatree.nodes :refer :all])
  (:import (aatree AASet)
           (clojure.lang MapEntry RT IPersistentMap)
           (aatree.nodes INode)))

(set! *warn-on-reflection* true)

(defn -getState [^AASet this]
  (.-state this))

(defn -init
  ([node opts]
   [[] (->noded-state node opts nil)])
  ([node opts meta]
   [[] (->noded-state node opts meta)]))

(defn -meta [^AASet this] (get-meta this))

(defn -withMeta [^AASet this meta] (new AASet (get-inode this) (get-opts this) meta))
