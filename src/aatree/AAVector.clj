(ns aatree.AAVector
  (:gen-class
    :main false
    :extends clojure.lang.APersistentVector
    :implements [clojure.lang.IObj aatree.nodes.flex_vector]
    :constructors {[aatree.nodes.INode]
                   []
                   [aatree.nodes.INode clojure.lang.IPersistentMap]
                   []}
    :init init
    :state state)
  (:require [aatree.nodes :refer :all])
  (:import (aatree AAVector)
           (aatree.nodes INode)))

(set! *warn-on-reflection* true)

(defrecord vector-state [node meta])

(defn- ^INode get-state-node [^AAVector this]
  (:node (.-state this)))

(defn -init
  ([node]
   [[] (->vector-state node nil)])
  ([node meta]
   [[] (->vector-state node meta)])
  )

(defn -meta [^AAVector this] (:meta (.-state this)))

(defn -withMeta [^AAVector this meta] (new AAVector (:node (.-state this)) meta))

(defn -count [this]
  (.getCnt (get-state-node this)))

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
    (new AAVector n1 (:meta (.-state this)))))

(defn -addNode [^AAVector this i val]
  (let [c (-count this)]
    (cond
      (= i c)
      (-cons this val)
      (and (>= i 0) (< i c))
      (let [n0 (:node (.-state this))
            n1 (vector-add n0 val i)]
        (new AAVector n1 (:meta (.-state this))))
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
        (new AAVector n1 (:meta (.-state this))))
      :else
      (throw (IndexOutOfBoundsException.)))))

(defn -empty [^AAVector this]
  (new AAVector (empty-node (:node (.-state this))) (:meta (.-state this))))

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
      (new AAVector n1 (:meta (.-state this))))))

(defn -dropNode [^AAVector this i]
  (if (or (< i 0) (>= i (-count this)))
    this
    (new AAVector (deln (:node (.-state this)) i) (:meta (.-state this)))))