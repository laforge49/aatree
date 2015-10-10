(ns aatree.AAMap
  (:gen-class
    :main false
    :extends clojure.lang.APersistentMap
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
  (:import (aatree AAMap)
           (clojure.lang MapEntry RT IPersistentMap)
           (aatree.nodes INode)))

(set! *warn-on-reflection* true)

(defn -getState [^AAMap this]
  (.-state this))

(defn- ^INode -getINode [this]
  (.-node (get-state this)))

(defn ^IPersistentMap -getOpts [this]
  (.opts (get-state this)))

(defn- ^IPersistentMap get-state-meta [this]
  (.-meta (get-state this)))

(defn -init
  ([node opts]
   [[] (->noded-state node opts nil)])
  ([node opts meta]
   [[] (->noded-state node opts meta)]))

(defn -meta [^AAMap this] (get-state-meta this))

(defn -withMeta [^AAMap this meta] (new AAMap (-getINode this) (-getOpts this) meta))

(defn -entryAt [^AAMap this key] (map-get-t2 (-getINode this) key (-getOpts this)))

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
  (let [n0 (-getINode this)
        n1 (map-insert n0 (new MapEntry key val) (-getOpts this))]
    (if (identical? n0 n1)
      this
      (new AAMap n1 (-getOpts this) (get-state-meta this)))))

(defn -assocEx [^AAMap this key val]
  (let [n0 (-getINode this)]
    (if (-containsKey this key)
      this
      (new AAMap
           (map-insert n0 (new MapEntry key val) (-getOpts this))
           (-getOpts this)
           (get-state-meta this)))))

(defn -without [^AAMap this key]
  (let [n0 (-getINode this)
        n1 (map-del n0 key (-getOpts this))]
    (if (identical? n0 n1)
      this
      (new AAMap n1 (-getOpts this) (get-state-meta this)))))

(defn -rseq [^AAMap this]
  (new-counted-reverse-seq (-getINode this) (-getOpts this)))

(defn -seq
  ([^AAMap this]
   (new-counted-seq (-getINode this) (-getOpts this)))
  ([this ascending]
   (if ascending
     (-seq this)
     (-rseq this))))

(defn -keyIterator [^AAMap this]
  (new-map-key-seq (-getINode this) (-getOpts this)))

(defn -valIterator [^AAMap this]
  (new-map-value-seq (-getINode this) (-getOpts this)))

(defn -seqFrom [^AAMap this key ascending]
  (if ascending
    (new-map-entry-seq (-getINode this) key (-getOpts this))
    (new-map-entry-reverse-seq (-getINode this) key (-getOpts this))))

(defn -empty [^AAMap this]
  (new AAMap (empty-node (-getINode this) (-getOpts this))
       (-getOpts this)
       (get-state-meta this)))

(defn -count [this]
  (.getCnt (-getINode this) (-getOpts this)))

(defn -entryKey [this ^MapEntry entry]
  (.getKey entry))

(defn -iterator [^AAMap this]
  (new-counted-iterator (-getINode this) (-getOpts this)))

(defn -nth
  ([^AAMap this i]
   (nth-t2 (-getINode this) i (-getOpts this)))
  ([this i notFound]
   (if (and (>= i 0) (< i (-count this)))
     (-nth this i)
     notFound)))
