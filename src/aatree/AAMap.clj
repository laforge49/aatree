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

(defn -init
  ([node opts]
   [[] (->noded-state node opts nil)])
  ([node opts meta]
   [[] (->noded-state node opts meta)]))

(defn -meta [^AAMap this] (get-meta this))

(defn -withMeta [^AAMap this meta] (new AAMap (get-inode this) (get-opts this) meta))

(defn -entryAt [^AAMap this key] (map-get-t2 (get-inode this) key (get-opts this)))

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
  (let [n0 (get-inode this)
        n1 (map-insert n0 (new MapEntry key val) (get-opts this))]
    (if (identical? n0 n1)
      this
      (new AAMap n1 (get-opts this) (get-meta this)))))

(defn -assocEx [^AAMap this key val]
  (let [n0 (get-inode this)]
    (if (-containsKey this key)
      this
      (new AAMap
           (map-insert n0 (new MapEntry key val) (get-opts this))
           (get-opts this)
           (get-meta this)))))

(defn -without [^AAMap this key]
  (let [n0 (get-inode this)
        n1 (map-del n0 key (get-opts this))]
    (if (identical? n0 n1)
      this
      (new AAMap n1 (get-opts this) (get-meta this)))))

(defn -rseq [^AAMap this]
  (new-counted-reverse-seq (get-inode this) (get-opts this)))

(defn -seq
  ([^AAMap this]
   (new-counted-seq (get-inode this) (get-opts this)))
  ([this ascending]
   (if ascending
     (-seq this)
     (-rseq this))))

(defn -seqFrom [^AAMap this key ascending]
  (if ascending
    (new-map-entry-seq (get-inode this) key (get-opts this))
    (new-map-entry-reverse-seq (get-inode this) key (get-opts this))))

(defn -empty [^AAMap this]
  (new AAMap (empty-node (get-inode this) (get-opts this))
       (get-opts this)
       (get-meta this)))

(defn -count [this]
  (.getCnt (get-inode this) (get-opts this)))

(defn -entryKey [this ^MapEntry entry]
  (.getKey entry))

(defn -iterator [^AAMap this]
  (new-counted-iterator (get-inode this) (get-opts this)))

(defn -nth
  ([^AAMap this i]
   (nth-t2 (get-inode this) i (get-opts this)))
  ([this i notFound]
   (if (and (>= i 0) (< i (-count this)))
     (-nth this i)
     notFound)))
