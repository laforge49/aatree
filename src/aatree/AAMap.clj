(ns aatree.AAMap
  (:gen-class
    :main false
    :extends clojure.lang.APersistentMap
    :implements [clojure.lang.IObj
                 clojure.lang.Reversible
                 clojure.lang.Sorted
                 clojure.lang.Counted
                 clojure.lang.Indexed]
    :constructors {[]
                   []
                   [java.util.Comparator]
                   []
                   [clojure.lang.IPersistentMap java.util.Comparator]
                   []
                   [clojure.lang.IPersistentMap java.lang.Object aatree.nodes.INode]
                   []}
    :init init
    :state state)
  (:require [aatree.nodes :refer :all]
            [aatree.map-nodes :refer :all])
  (:import (aatree AAMap)
           (clojure.lang MapEntry RT)))

(set! *warn-on-reflection* true)

(defrecord map-state [node meta comparator])

(defn -init
  ([]
   [[] (->map-state (create-empty-map-node) nil RT/DEFAULT_COMPARATOR)])
  ([comp]
   [[] (->map-state (create-empty-map-node comp) nil comp)])
  ([meta comp]
   [[] (->map-state (create-empty-map-node comp) meta comp)])
  ([meta comp node]
   [[] (->map-state node meta comp)]))

(defn -meta [^AAMap this] (:meta (.-state this)))

(defn -withMeta [^AAMap this meta] (new AAMap meta (:comparator (.-state this)) (:node (.-state this))))

(defn -entryAt [^AAMap this key] (get-t2 (:node (.-state this)) key (:comparator (.-state this))))

(defn -containsKey [this key] (boolean (-entryAt this key)))

(defn -valAt
  ([this key default]
   (let [^MapEntry e (-entryAt this key)]
     (if (nil? e)
       default
       (.getValue e))))
  ([this key]
   (-valAt this key nil)))

(defn -assoc [^AAMap this key val]
  (let [n0 (:node (.-state this))
        n1 (insert n0 (new MapEntry key val) (:comparator (.-state this)))]
    (if (identical? n0 n1)
      this
      (new AAMap (:meta (.-state this)) (:comparator (.-state this)) n1))))

(defn -assocEx [^AAMap this key val]
  (let [n0 (:node (.-state this))]
    (if (-containsKey this key)
      this
      (new AAMap (:meta (.-state this)) (:comparator (.-state this)) (insert n0 (new MapEntry key val) (:comparator (.-state this)))))))

(defn -without [^AAMap this key]
  (let [n0 (:node (.-state this))
        n1 (del n0 key (:comparator (.-state this)))]
    (if (identical? n0 n1)
      this
      (new AAMap (:meta (.-state this)) (:comparator (.-state this)) n1))))

(defn -rseq [^AAMap this]
  (new-counted-reverse-seq (:node (.-state this))))

(defn -seq
  ([^AAMap this]
   (new-counted-seq (:node (.-state this))))
  ([this ascending]
   (if ascending
     (-seq this)
     (-rseq this))))

(defn -keyIterator [^AAMap this]
  (new-map-key-seq (:node (.-state this))))

(defn -valIterator [^AAMap this]
  (new-map-value-seq (:node (.-state this))))

(defn -seqFrom [^AAMap this key ascending]
  (if ascending
    (new-map-entry-seq (:node (.-state this)) key (:comparator (.-state this)))
    (new-map-entry-reverse-seq (:node (.-state this)) key (:comparator (.-state this)))))

(defn -empty [^AAMap this]
  (new AAMap (:meta (.-state this)) (:comparator (.-state this)) (empty-node (:node (.-state this)))))

(defn -count [^AAMap this]
  (:cnt (:node (.-state this))))

(defn -comparator [^AAMap this]
  (:comparator (:node (.-state this))))

(defn -entryKey [this ^MapEntry entry]
  (.getKey entry))

(defn -iterator [^AAMap this]
  (new-counted-iterator (:node (.-state this))))

(defn -nth
  ([^AAMap this i]
   (nth-t2 (:node (.-state this)) i))
  ([this i notFound]
   (if (and (>= i 0) (< i (-count this)))
     (-nth this i)
     notFound)))
