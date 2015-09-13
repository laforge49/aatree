(ns aatree.lazy-nodes
  (:require [aatree.nodes :refer :all])
  (:import (aatree.nodes INode)))

(set! *warn-on-reflection* true)

(declare ->LazyNode
         ^INode get-data
         get-factory)

(deftype LazyNode [data-atom buffer-atom factory-registry factory]

  INode

  (newNode [this t2 level left right cnt]
    (let [d (->Node t2 level left right cnt (empty-node (get-data this)))]
      (->LazyNode d (atom nil) factory-registry (get-factory factory-registry t2))))

  (getT2 [this] (.getT2 (get-data this)))

  (getLevel [this] (.getLevel (get-data this)))

  (getLeft [this] (.getLeft (get-data this)))

  (getRight [this] (.getRight (get-data this)))

  (getCnt [this] (.getCnt (get-data this)))

  (getNada [this] (.getNada (get-data this))))

(definterface IFactory
  (byteLength [lazyNode])
  (asData [lazyNode])
  (write [buffer])
  (read [buffer]))

(defn- ^IFactory get-factory
  ([^LazyNode lazy-node]
   (.-factory lazy-node))
  ([factory-registry t2]
   nil))

(defn- deserialize [^LazyNode this]
  (let [d (.asData (get-factory this) this)
        a (.-data-atom this)]
    (compare-and-set! a nil d)
    @a))

(defn- ^INode get-data [^LazyNode this]
  (let [d @(.-data-atom this)]
    (if (nil? d)
      (deserialize this)
      d)))

(defn create-lazy-empty-node
  ([factory-registry] (->LazyNode (atom (create-empty-node)) (atom nil) factory-registry nil)))
