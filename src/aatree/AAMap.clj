(ns aatree.AAMap
  (:gen-class
   :main false
   :extends clojure.lang.APersistentMap
   :implements [clojure.lang.IObj
                clojure.lang.Reversible
                clojure.lang.Sorted
                clojure.lang.Counted
                clojure.lang.Indexed]
   :constructors {[aatree.nodes.INode clojure.lang.IPersistentMap]
                  []
                  [aatree.nodes.INode clojure.lang.IPersistentMap clojure.lang.IPersistentMap]
                  []}
   :init init
   :state state)
  (:require [aatree.nodes :refer :all])
  (:import (aatree AAMap)
           (clojure.lang MapEntry RT IPersistentMap)
           (aatree.nodes INode)))

(set! *warn-on-reflection* true)

(deftype map-state [node resources meta])

(defn ^map-state get-state [^AAMap this]
  (.-state this))

(defn- ^INode get-state-node [this]
  (.-node (get-state this)))

(defn- ^IPersistentMap get-state-resources [this]
  (.-resources (get-state this)))

(defn- ^IPersistentMap get-state-meta [this]
  (.-meta (get-state this)))

(defn -init
  ([node resources]
   [[] (->map-state node resources nil)])
  ([node resources meta]
   [[] (->map-state node resources meta)]))

(defn -meta [^AAMap this] (get-state-meta this))

(defn -withMeta [^AAMap this meta] (new AAMap (get-state-node this) (get-state-resources this) meta))

(defn -entryAt [^AAMap this key] (map-get-t2 (get-state-node this) key (get-state-resources this)))

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
  (let [n0 (get-state-node this)
        n1 (map-insert n0 (new MapEntry key val) (get-state-resources this))]
    (if (identical? n0 n1)
      this
      (new AAMap n1 (get-state-resources this) (get-state-meta this)))))

(defn -assocEx [^AAMap this key val]
  (let [n0 (get-state-node this)]
    (if (-containsKey this key)
      this
      (new AAMap
           (map-insert n0 (new MapEntry key val) (get-state-resources this))
           (get-state-resources this)
           (get-state-meta this)))))

(defn -without [^AAMap this key]
  (let [n0 (get-state-node this)
        n1 (map-del n0 key (get-state-resources this))]
    (if (identical? n0 n1)
      this
      (new AAMap n1 (get-state-resources this) (get-state-meta this)))))

(defn -rseq [^AAMap this]
  (new-counted-reverse-seq (get-state-node this)))

(defn -seq
  ([^AAMap this]
   (new-counted-seq (get-state-node this)))
  ([this ascending]
   (if ascending
     (-seq this)
     (-rseq this))))

(defn -keyIterator [^AAMap this]
  (new-map-key-seq (get-state-node this)))

(defn -valIterator [^AAMap this]
  (new-map-value-seq (get-state-node this)))

(defn -seqFrom [^AAMap this key ascending]
  (if ascending
    (new-map-entry-seq (get-state-node this) key (get-state-resources this))
    (new-map-entry-reverse-seq (get-state-node this) key (get-state-resources this))))

(defn -empty [^AAMap this]
  (new AAMap (empty-node (get-state-node this))
       (get-state-resources this)
       (get-state-meta this)))

(defn -count [this]
  (.getCnt (get-state-node this)))

(defn -entryKey [this ^MapEntry entry]
  (.getKey entry))

(defn -iterator [^AAMap this]
  (new-counted-iterator (get-state-node this)))

(defn -nth
  ([^AAMap this i]
   (nth-t2 (get-state-node this) i))
  ([this i notFound]
   (if (and (>= i 0) (< i (-count this)))
     (-nth this i)
     notFound)))
