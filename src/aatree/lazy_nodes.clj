(ns aatree.lazy-nodes
  (:require [aatree.nodes :refer :all])
  (:import (aatree.nodes INode)))

(set! *warn-on-reflection* true)

(declare ->LazyNode)

(deftype LazyNode [^INode data]

  INode

  (newNode [this t2 level left right cnt]
    (let [d (->Node t2 level left right cnt (empty-node data))]
      (->LazyNode d)))

  (getT2 [this] (.getT2 data))

  (getLevel [this] (.getLevel data))

  (getLeft [this] (.getLeft data))

  (getRight [this] (.getRight data))

  (getCnt [this] (.getCnt data))

  (getNada [this] (.getNada data)))

(defn create-lazy-empty-node
  ([] (->LazyNode (create-empty-node))))
