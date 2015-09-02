(ns aatree.AAMap
  (:gen-class
    :main false
    :extends clojure.lang.APersistentMap
    :implements [clojure.lang.IObj]
    :constructors {[]
                   []
                   [java.util.Comparator]
                   []
                   [clojure.lang.IPersistentMap java.util.Comparator]
                   []
                   [clojure.lang.IPersistentMap java.lang.Object aatree.nodes.IMapNode]
                   []}
    :init init
    :state state
    )
    (:require [aatree.nodes :refer :all])
    (:import (aatree AAMap)))


(deftype map-state [node meta])

(defn -init
  ([]
   [[] (->map-state (emty-node) nil)])
  ([comp]
   [[] (->map-state (emty-node comp) nil)])
  ([meta comp]
   [[] (->map-state (emty-node comp) meta)])
  ([meta _ node]
   [[] (->map-state node meta)]))

(defn -meta [this] (.-meta (.-state this)))

(defn -withMeta [this meta] (new AAMap meta nil (.-node (.-state this))))

(defn -entryAt [this key] (.get-t2 (.-node (.-state this)) key))

(defn -containsKey [this key] (boolean (-entryAt this key)))

(defn -valAt
  ([this key default]
   (let [e (-entryAt this key)]
     (if (nil? e)
       default
       (.getValue e))))
  ([this key]
   (-valAt this key nil)))
