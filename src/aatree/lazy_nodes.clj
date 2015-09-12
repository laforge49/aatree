(ns aatree.lazy-nodes
  (:require [aatree.nodes :refer :all])
  (:import (aatree.nodes INode)))

(set! *warn-on-reflection* true)

(declare ->LazyNode)

(deftype LazyNode [t2 ^int level left right ^int cnt nada]

  INode

  (newNode [this t2 level left right cnt]
    (->LazyNode t2 level left right cnt (empty-node this)))

  (getT2 [this] t2)

  (getLevel [this] level)

  (getLeft [this] left)

  (getRight [this] right)

  (getCnt [this] cnt)

  (getNada [this] nada))

(defn create-lazy-empty-node
  ([] (->LazyNode nil 0 nil nil 0 nil)))
