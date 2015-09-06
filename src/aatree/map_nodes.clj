(ns aatree.map-nodes
  (:require [aatree.nodes :refer :all])
  (:import (java.util Comparator)
           (clojure.lang RT IMapEntry Counted MapEntry)))

(declare ->MapNode)

(deftype MapNode [^IMapEntry t2 ^int level left right ^int cnt ^Comparator comparator nada]

  Counted

  (count [this]
    (if (empty-node? this)
      0
      cnt))

  IMapNode

  (emty [this]
    (if (empty-node? this)
      this
      nada))

  (cmpr [this x]
    (.compare comparator x (.getKey t2)))

  (right-node [this]
    (if (empty-node? right)
      (.emty this)
      right))

  (left-node [this]
    (if (empty-node? (.-left this))
      (.emty this)
      (.-left this)))

  (new-node [this t2 level left right cnt]
    (->MapNode t2 level left right cnt (.-comparator this) (.emty this)))

  (revise [this args]
    (let [m (apply array-map args)
          t-2 (get m :t2 t2)
          lev (get m :level level)
          l (get m :left (.left-node this))
          r (get m :right (.right-node this))
          c (+ 1 (.count l) (.count r))]
      (if (and (= (.getKey t-2) (.getKey t2))
               (identical? (.getValue t-2) (.getValue t2))
               (= lev level)
               (identical? l (.left-node this))
               (identical? r (.right-node this)))
        this
        (.new-node this t-2 lev l r c))))

  (skew
    [this]
    (cond
      (empty-node? this)
      this
      (empty-node? left)
      this
      (= (.-level left) level)
      (let [l left]
        (.revise l [:right (.revise this [:left (.right-node l)])]))
      :else
      this))

  (split [this]
    (cond
      (empty-node? this)
      this
      (or (empty-node? right) (empty-node? (.-right right)))
      this
      (= level (.-level (.-right right)))
      (.revise right
               [:level (+ 1 (.-level right))
                :left (.revise this [:right (.-left right)])])
      :else
      this))

  (insert [this t-2]
    (if (empty-node? this)
      (.new-node this t-2 1 nil nil 1)
      (let [c (.cmpr this (.getKey t-2))]
        (.split (.skew (cond
                         (< c 0)
                         (let [oldl (.left-node this)
                               l (.insert oldl t-2)]
                           (.revise this [:left l]))
                         (> c 0)
                         (let [oldr (.right-node this)
                               r (.insert oldr t-2)]
                           (.revise this [:right r]))
                         :else
                         (.revise this [:t2 (new MapEntry (.getKey t2) (.getValue t-2))])))))))

  (predecessor-t2 [this]
    (last-t2 (.left-node this)))

  (successor-t2 [this]
    (first-t2 (.right-node this)))

  (next-t2 [this x]
    (if (empty-node? this)
      nil
      (let [c (.cmpr this x)]
        (cond
          (zero? c) (.successor-t2 this)
          (> c 0) (.next-t2 (.right-node this) x)
          :else (let [t-2 (.next-t2 (.left-node this) x)]
                  (if (nil? t-2)
                    t2
                    t-2))))))

  (prior-t2 [this x]
    (if (empty-node? this)
      nil
      (let [c (.cmpr this x)]
        (cond
          (zero? c) (.predecessor-t2 this)
          (< c 0) (.prior-t2 (.left-node this) x)
          :else (let [t-2 (.prior-t2 (.right-node this) x)]
                  (if (nil? t-2)
                    t2
                    t-2))))))

  (get-t2 [this x]
    (if (empty-node? this)
      nil
      (let [c (.cmpr this x)]
        (cond
          (zero? c) t2
          (> c 0) (.get-t2 (.right-node this) x)
          :else (.get-t2 (.left-node this) x)))))

  (decrease-level [this]
    (let [should-be (+ 1 (min (.-level (.left-node this))
                              (.-level (.right-node this))))]
      (if (>= should-be level)
        this
        (let [rn (.right-node this)
              rn (if (>= should-be (.-level (.right-node this)))
                   rn
                   (.revise rn [:level should-be]))]
          (.revise this [:right rn :level should-be])))))

  (delete [this x]
    (if (empty-node? this)
      this
      (let [c (.cmpr this x)]
        (if (and (= c 0) (= 1 level))
          (.right-node this)
          (let [t (cond
                    (> c 0)
                    (.revise this [:right (.delete (.right-node this) x)])
                    (< c 0)
                    (.revise this [:left (.delete (.left-node this) x)])
                    :else
                    (let [p (.predecessor-t2 this)]
                      (.revise this [:t2 p :left (.delete (.left-node this) (.getKey p))])))
                t (.decrease-level t)
                t (.skew t)
                t (.revise t [:right (.skew (.right-node t))])
                r (.right-node t)
                t (if (empty-node? r)
                    t
                    (.revise t [:right (.revise r [:right (.skew (.right-node r))])]))
                t (.split t)
                t (.revise t [:right (.split (.right-node t))])]
            t)))))

  (index-of [this x]
    (if (empty-node? this)
      0
      (let [c (.cmpr this x)]
        (cond
          (< c 0)
          (.index-of (.left-node this) x)
          (= c 0)
          (.-cnt (.left-node this))
          :else
          (+ 1
             (.-cnt (.left-node this))
             (.index-of (.right-node this) x))))))

  (nth-t2 [this i]
    (if (empty-node? this)
      (throw (IndexOutOfBoundsException.))
      (let [l (.left_node this)
            p (.-cnt l)]
        (cond
          (< i p)
          (.nth-t2 l i)
          (> i p)
          (.nth-t2 (.right_node this) (- i p 1))
          :else
          t2))))
  )

(defn create-empty-map-node
  ([] (create-empty-map-node RT/DEFAULT_COMPARATOR))
  ([^Comparator comparator] (->MapNode nil 0 nil nil 0 comparator nil)))