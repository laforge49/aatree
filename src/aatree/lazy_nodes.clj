(ns aatree.lazy-nodes
  (:require [aatree.nodes :refer :all])
  (:import (aatree.nodes INode)))

(set! *warn-on-reflection* true)

(declare ->LazyNode
         ^INode get-data)

(deftype LazyNode [^INode data]

  INode

  (newNode [this t2 level left right cnt]
    (let [d (->Node t2 level left right cnt (empty-node data))]
      (->LazyNode d)))

  (getT2 [this] (.getT2 (get-data this)))

  (getLevel [this] (.getLevel (get-data this)))

  (getLeft [this] (.getLeft (get-data this)))

  (getRight [this] (.getRight (get-data this)))

  (getCnt [this] (.getCnt (get-data this)))

  (getNada [this] (.getNada (get-data this))))

(defn- ^INode get-data [^LazyNode this]
  (.-data this))

(defn create-lazy-empty-node
  ([] (->LazyNode (create-empty-node))))
