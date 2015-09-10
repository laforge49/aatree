(ns aatree.AAVector
  (:gen-class
    :main false
    :extends clojure.lang.APersistentVector
    :implements [clojure.lang.IObj]
    :constructors {[]
                   []
                   [clojure.lang.IPersistentMap]
                   []
                   [clojure.lang.IPersistentMap aatree.nodes.INode]
                   []}
    :init init
    :state state)
  (:require [aatree.nodes :refer :all]
            [aatree.vector-nodes :refer :all])
  (:import (aatree AAVector)))

(set! *warn-on-reflection* true)

(defrecord vector-state [node meta])

(defn -init
  ([]
   [[] (->vector-state (create-empty-vector-node) nil)])
  ([meta]
   [[] (->vector-state (create-empty-vector-node) meta)])
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
        n1 (node-add n0 val (-count this))]
    (if (identical? n0 n1)
      this
      (new AAVector (:meta (.-state this)) n1))
  ))

(defn -assocN [^AAVector this i val]
  (let [c (-count this)]
    (cond
      (= i c)
      (-cons this val)
      (and (>= i 0) (< i c))
      (let [n0 (:node (.-state this))
            n1 (node-add (:node (.-state this)) val i)]
        (if (identical? n0 n1)
          this
          (new AAVector (:meta (.-state this)) n1))
        )
      :else
      (throw (IndexOutOfBoundsException.)))))

(defn -empty [^AAVector this]
  (new AAVector (:meta (.-state this)) nil (empty-node (:node (.-state this)))))
