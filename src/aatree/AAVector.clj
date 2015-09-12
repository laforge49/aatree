(ns aatree.AAVector
  (:gen-class
    :main false
    :extends clojure.lang.APersistentVector
    :implements [clojure.lang.IObj aatree.nodes.flex_vector]
    :constructors {[]
                   []
                   [clojure.lang.IPersistentMap]
                   []
                   [clojure.lang.IPersistentMap aatree.nodes.INode]
                   []}
    :init init
    :state state)
  (:require [aatree.nodes :refer :all])
  (:import (aatree AAVector)))

(set! *warn-on-reflection* true)

(defrecord vector-state [node meta])

(defn -init
  ([]
   [[] (->vector-state (create-empty-node) nil)])
  ([meta]
   [[] (->vector-state (create-empty-node) meta)])
  ([meta node]
   [[] (->vector-state node meta)]))

(defn -meta [^AAVector this] (:meta (.-state this)))

(defn -withMeta [^AAVector this meta] (new AAVector meta (:node (.-state this))))

(defn -count [^AAVector this]
  (:cnt (:node (.-state this))))

(defn -nth
  ([^AAVector this i]
   (nth-t2 (:node (.-state this)) i))
  ([this i notFound]
   (if (and (>= i 0) (< i (-count this)))
     (-nth this i)
     notFound)))

(defn -cons [^AAVector this val]
  (let [n0 (:node (.-state this))
        n1 (vector-add n0 val (-count this))]
    (new AAVector (:meta (.-state this)) n1)))

(defn -addNode [^AAVector this i val]
  (let [c (-count this)]
    (cond
      (= i c)
      (-cons this val)
      (and (>= i 0) (< i c))
      (let [n0 (:node (.-state this))
            n1 (vector-add n0 val i)]
        (new AAVector (:meta (.-state this)) n1))
      :else
      (throw (IndexOutOfBoundsException.)))))

(defn -assocN [^AAVector this i val]
  (let [c (-count this)]
    (cond
      (= i c)
      (-cons this val)
      (and (>= i 0) (< i c))
      (let [n0 (:node (.-state this))
            n1 (vector-set n0 val i)]
        (new AAVector (:meta (.-state this)) n1))
      :else
      (throw (IndexOutOfBoundsException.)))))

(defn -empty [^AAVector this]
  (new AAVector (:meta (.-state this)) (empty-node (:node (.-state this)))))

(defn -iterator [^AAVector this]
  (new-counted-iterator (:node (.-state this))))

(defn -seq
  [^AAVector this]
   (new-counted-seq (:node (.-state this))))

(defn -pop [^AAVector this]
  (if (empty? this)
    this
    (let [n0 (:node (.-state this))
          n1 (deln n0 (- (-count this) 1))]
      (new AAVector (:meta (.-state this)) n1))))

(defn -dropNode [^AAVector this i]
  (if (or (< i 0) (>= i (-count this)))
    this
    (new AAVector (:meta (.-state this)) (deln (:node (.-state this)) i))))