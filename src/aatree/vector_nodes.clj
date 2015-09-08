(ns aatree.vector-nodes
  (:require [aatree.nodes :refer :all])
  (:import (clojure.lang RT Counted)
           (aatree nodes.counted-iterator nodes.counted-reverse-iterator CountedSequence)))

(declare ->VectorNode)

(defrecord VectorNode [t2 ^int level left right ^int cnt nada]

  INode

  (new-node [this t2 level left right cnt]
    (->VectorNode t2 level left right cnt (empty-node this)))
  )

(defn create-empty-vector-node
  ([] (->VectorNode nil 0 nil nil 0 nil)))

(defn ^counted-iterator new-vector-iterator
  ([node i]
   (->counted-iterator node i (:cnt node)))
  )

(defn ^CountedSequence new-vector-seq
  ([node i]
   (CountedSequence/create (new-vector-iterator node i) identity)))

(defn ^counted-reverse-iterator new-vector-reverse-iterator
  ([node i]
   (->counted-reverse-iterator node i))
  )

(defn ^CountedSequence new-vector-reverse-seq
  ([node i]
   (CountedSequence/create (new-vector-reverse-iterator node i) identity)))

(defn node-add [^VectorNode n v i]
  (if (empty-node? n)
    (.new-node n v 1 nil nil 1)
    (let [l (left-node n)
          p (:cnt l)]
      (split
        (skew
          (if (<= i p)
            (revise n [:left (node-add l v i)])
            (revise n [:right (node-add (right-node n) v (- i p 1))])))))))
