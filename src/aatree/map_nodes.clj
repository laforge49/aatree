(ns aatree.map-nodes
  (:require [aatree.nodes :refer :all])
  (:import (java.util Comparator)
           (clojure.lang RT IMapEntry Counted MapEntry)
           (aatree nodes.counted-iterator nodes.counted-reverse-iterator CountedSequence)
           (aatree.nodes INode)))

(set! *warn-on-reflection* true)

(declare ->MapNode)

(defrecord MapNode [^IMapEntry t2 ^int level left right ^int cnt nada]

  INode

  (newNode [this t2 level left right cnt]
    (->MapNode t2 level left right cnt (empty-node this)))
  )

(defn create-empty-map-node
  ([] (->MapNode nil 0 nil nil 0 nil)))

(defn ^MapEntry get-entry [this] (:t2 this))

(defn key-of [^IMapEntry e] (.getKey e))

(defn value-of [^IMapEntry e] (.getValue e))

(defn cmpr [this x ^Comparator comparator]
      (.compare comparator x (.getKey (get-entry this))))

(defn index-of [this x comparator]
  (if (empty-node? this)
    0
    (let [c (cmpr this x comparator)]
      (cond
        (< c 0)
        (index-of (left-node this ) x comparator)
        (= c 0)
        (:cnt (left-node this))
        :else
        (+ 1
           (:cnt (left-node this))
           (index-of (right-node this) x comparator))))))

(defn ^counted-iterator new-map-entry-iterator
  ([node x comparator]
   (->counted-iterator node (index-of node x comparator) (:cnt node)))
  )

(defn ^CountedSequence new-map-entry-seq
  ([node x comparator]
   (CountedSequence/create (new-map-entry-iterator node x comparator) identity)))

(defn ^CountedSequence new-map-key-seq [node]
  (CountedSequence/create (new-counted-iterator node) key-of))

(defn ^CountedSequence new-map-value-seq [node]
  (CountedSequence/create (new-counted-iterator node) value-of))

(defn ^counted-reverse-iterator new-map-entry-reverse-iterator
  ([node x comparator]
   (->counted-reverse-iterator node (index-of node x comparator)))
  )

(defn ^CountedSequence new-map-entry-reverse-seq
  ([node x comparator]
   (CountedSequence/create (new-map-entry-reverse-iterator node x comparator) identity)))

(defn ^CountedSequence new-map-key-reverse-seq [node]
  (CountedSequence/create (new-counted-reverse-iterator node) key-of))

(defn ^CountedSequence new-map-value-reverse-seq [node]
  (CountedSequence/create (new-counted-reverse-iterator node) value-of))

(defn insert [^MapNode this ^MapEntry t-2 comparator]
        (if (empty-node? this)
          (.newNode this t-2 1 nil nil 1)
          (let [c (cmpr this (.getKey t-2) comparator)]
            (split (skew (cond
                             (< c 0)
                             (let [oldl (left-node this)
                                   l (insert oldl t-2 comparator)]
                               (revise this [:left l]))
                             (> c 0)
                             (let [oldr (right-node this)
                                   r (insert oldr t-2 comparator)]
                               (revise this [:right r]))
                             :else
                             (if (identical? (.getValue t-2)(.getValue (get-entry this)))
                               this
                               (revise this [:t2 (new MapEntry (.getKey (get-entry this)) (.getValue t-2))]))))))))

(defn get-t2 [this x comparator]
        (if (empty-node? this)
          nil
          (let [c (cmpr this x comparator)]
            (cond
              (zero? c) (:t2 this)
              (> c 0) (get-t2 (right-node this) x comparator)
              :else (get-t2 (left-node this) x comparator)))))

(defn del [this x comparator]
  (if (empty-node? this)
    this
    (let [c (cmpr this x comparator)]
      (if (and (= c 0) (= 1 (:level this)))
        (right-node this)
        (let [t (cond
                  (> c 0)
                  (revise this [:right (del (right-node this) x comparator)])
                  (< c 0)
                  (revise this [:left (del (left-node this) x comparator)])
                  :else
                  (let [^MapEntry p (predecessor-t2 this)]
                    (revise this [:t2 p :left (del (left-node this) (.getKey p) comparator)])))
              t (decrease-level t)
              t (skew t)
              t (revise t [:right (skew (right-node t))])
              r (right-node t)
              t (if (empty-node? r)
                  t
                  (revise t [:right (revise r [:right (skew (right-node r))])]))
              t (split t)
              t (revise t [:right (split (right-node t))])]
          t)))))
