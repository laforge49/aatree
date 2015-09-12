(ns aatree.AAVector
  (:gen-class
   :main false
   :extends clojure.lang.APersistentVector
   :implements [clojure.lang.IObj aatree.nodes.FlexVector]
   :constructors {[aatree.nodes.INode]
                  []
                  [aatree.nodes.INode clojure.lang.IPersistentMap]
                  []}
   :init init
   :state state)
  (:require [aatree.nodes :refer :all])
  (:import (aatree AAVector)
           (aatree.nodes INode)
           (clojure.lang IPersistentMap)))

(set! *warn-on-reflection* true)

(deftype vector-state [node meta])

(defn ^vector-state get-state [^AAVector this]
  (.-state this))

(defn- ^INode get-state-node [this]
  (.-node (get-state this)))

(defn- ^IPersistentMap get-state-meta [this]
  (.-meta (get-state this)))

(defn -init
  ([node]
   [[] (->vector-state node nil)])
  ([node meta]
   [[] (->vector-state node meta)]))

(defn -meta [^AAVector this] (get-state-meta this))

(defn -withMeta [^AAVector this meta] (new AAVector (get-state-node this) meta))

(defn -count [this]
  (.getCnt (get-state-node this)))

(defn -nth
  ([^AAVector this i]
   (nth-t2 (get-state-node this) i))
  ([this i notFound]
   (if (and (>= i 0) (< i (-count this)))
     (-nth this i)
     notFound)))

(defn -cons [^AAVector this val]
  (let [n0 (get-state-node this)
        n1 (vector-add n0 val (-count this))]
    (new AAVector n1 (get-state-meta this))))

(defn -addNode [^AAVector this i val]
  (let [c (-count this)]
    (cond
      (= i c)
      (-cons this val)
      (and (>= i 0) (< i c))
      (let [n0 (get-state-node this)
            n1 (vector-add n0 val i)]
        (new AAVector n1 (get-state-meta this)))
      :else
      (throw (IndexOutOfBoundsException.)))))

(defn -assocN [^AAVector this i val]
  (let [c (-count this)]
    (cond
      (= i c)
      (-cons this val)
      (and (>= i 0) (< i c))
      (let [n0 (get-state-node this)
            n1 (vector-set n0 val i)]
        (new AAVector n1 (get-state-meta this)))
      :else
      (throw (IndexOutOfBoundsException.)))))

(defn -empty [^AAVector this]
  (new AAVector (empty-node (get-state-node this)) (get-state-meta this)))

(defn -iterator [^AAVector this]
  (new-counted-iterator (get-state-node this)))

(defn -seq
  [^AAVector this]
  (new-counted-seq (get-state-node this)))

(defn -pop [^AAVector this]
  (if (empty? this)
    this
    (let [n0 (get-state-node this)
          n1 (deln n0 (- (-count this) 1))]
      (new AAVector n1 (get-state-meta this)))))

(defn -dropNode [^AAVector this i]
  (if (or (< i 0) (>= i (-count this)))
    this
    (new AAVector (deln (get-state-node this) i) (get-state-meta this))))