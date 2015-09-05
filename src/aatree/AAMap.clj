(ns aatree.AAMap
  (:gen-class
    :main false
    :extends clojure.lang.APersistentMap
    :implements [clojure.lang.IObj
                 clojure.lang.Reversible
                 clojure.lang.Sorted
                 clojure.lang.Counted
                 clojure.lang.IMapIterable]
    :constructors {[]
                   []
                   [java.util.Comparator]
                   []
                   [clojure.lang.IPersistentMap java.util.Comparator]
                   []
                   [clojure.lang.IPersistentMap java.lang.Object aatree.nodes.IMapNode]
                   []}
    :init init
    :state state)
  (:require [aatree.nodes :refer :all])
  (:import (aatree AAMap)
           (clojure.lang MapEntry)))


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

(defn -assoc [this key val]
  (let [n0 (.-node (.-state this))
        n1 (.insert n0 (new MapEntry key val))]
    (if (identical? n0 n1)
      this
      (new AAMap (.-meta (.-state this)) nil n1))))

(defn -assocEx [this key val]
  (let [n0 (.-node (.-state this))]
    (if (-containsKey this key)
      this
      (new AAMap (.-meta (.-state this)) nil (.insert n0 (new MapEntry key val))))))

(defn -without [this key]
  (let [n0 (.-node (.-state this))
        n1 (.delete n0 key)]
    (if (identical? n0 n1)
      this
      (new AAMap (.-meta (.-state this)) nil n1))))

(defn -rseq [this]
  (new-map-entry-reverse-seq (.-node (.-state this))))

(defn -seq
  ([this]
   (new-map-entry-seq (.-node (.-state this))))
  ([this ascending]
   (if ascending
     (-seq this)
     (-rseq this))))

(defn -keyIterator [this]
  (new-map-key-seq (.-node (.-state this))))

(defn -valIterator [this]
  (new-map-value-seq (.-node (.-state this))))

(defn -seqFrom [this key ascending]
  (if ascending
    (new-map-entry-seq (.-node (.-state this)) key)
    (new-map-entry-reverse-seq (.-node (.-state this)) key)))

(defn -empty [this]
  (new AAMap (.-meta (.-state this)) nil (.emty (.-node (.-state this)))))

(defn -count [this]
  (.-cnt (.-node (.-state this))))

(defn -comparator [this]
  (.-comparator (.-node (.-state this))))

(defn -entryKey [this entry]
  (.getKey entry))

(defn -iterator [this]
  (new-map-entry-iterator (.-node (.-state this))))
