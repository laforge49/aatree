(ns aatree.nodes
  (:import (clojure.lang Counted MapEntry IMapEntry)
           (java.util Iterator Comparator)
           (aatree CountedSequence)))

(set! *warn-on-reflection* true)

(definterface INode
  ; t2 ^int level left right ^int cnt nada
  (newNode [t2 ^int level left right ^int cnt])
  (getT2 [])
  (getLevel [])
  (get_left [])
  (get_right [])
  (get_cnt [])
  (get_nada [])
  )

(defn empty-node? [^INode n]
  (or (nil? n) (zero? (.getLevel n))))

(defn last-t2 [^INode this]
  (cond
    (empty-node? this) nil
    (empty-node? (.get-right this)) (.getT2 this)
    :else (recur (.get-right this))))

(defn empty-node [^INode this]
  (if (empty-node? this)
    this
    (.get-nada this)))

(defn left-node [^INode this]
  (if (empty-node? (.get-left this))
    (empty-node this)
    (.get-left this)))

(defn right-node [^INode this]
  (if (empty-node? (.get-right this))
    (empty-node this)
    (.get-right this)))

(defn node-count [^INode this]
  (if (empty-node? this)
    0
    (.get-cnt this)))

(defn revise [^INode this args]
  (let [m (apply array-map args)
        t-2 (get m :t2 (.getT2 this))
        lev (get m :level (.getLevel this))
        l (get m :left (left-node this))
        r (get m :right (right-node this))
        c (+ 1 (node-count l) (node-count r))]
    (if (and (identical? t-2 (.getT2 this))
             (= lev (.getLevel this))
             (identical? l (left-node this))
             (identical? r (right-node this)))
      this
      (.newNode this t-2 lev l r c))))

(defn skew
  [^INode this]
  (cond
    (empty-node? this)
    this
    (empty-node? (.get-left this))
    this
    (= (.getLevel (.get-left this)) (.getLevel this))
    (let [l (.get-left this)]
      (revise l [:right (revise this [:left (right-node l)])]))
    :else
    this))

(defn split [^INode this]
  (cond
    (empty-node? this)
    this
    (or (empty-node? (.get-right this)) (empty-node? (.get-right (.get-right this))))
    this
    (= (.getLevel this) (.getLevel (.get-right (.get-right this))))
    (revise (.get-right this)
            [:level (+ 1 (.getLevel (.get-right this)))
             :left (revise this [:right (.get-left (.get-right this))])])
    :else
    this))

(defn predecessor-t2 [this]
  (last-t2 (left-node this)))

(defn decrease-level [^INode this]
  (let [should-be (+ 1 (min (.getLevel (left-node this))
                            (.getLevel (right-node this))))]
    (if (>= should-be (.getLevel this))
      this
      (let [rn (right-node this)
            rn (if (>= should-be (.getLevel (right-node this)))
                 rn
                 (revise rn [:level should-be]))]
        (revise this [:right rn :level should-be])))))

(defn nth-t2 [^INode this i]
  (if (empty-node? this)
    (throw (IndexOutOfBoundsException.))
    (let [l (left-node this)
          p (.get-cnt l)]
      (cond
        (< i p)
        (nth-t2 l i)
        (> i p)
        (nth-t2 (right-node this) (- i p 1))
        :else
        (.getT2 this)))))

(defn deln [^INode this i]
  (if (empty-node? this)
    this
    (let [l (left-node this)
          p (.get-cnt l)]
      (if (and (= i p) (= 1 (.getLevel this)))
        (right-node this)
        (let [t (cond
                  (> i p)
                  (revise this [:right (deln (right-node this) (- i p 1))])
                  (< i p)
                  (revise this [:left (deln (left-node this) i)])
                  :else
                  (let [pre (predecessor-t2 this)]
                    (revise this [:t2 pre :left (deln (left-node this) (- i 1))])))
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

(deftype counted-iterator
  [node
   ^{:volatile-mutable true int true} ndx
   ^int cnt]

  Counted
  (count [this] (- cnt ndx))

  Iterator
  (hasNext [this]
    (< ndx cnt))
  (next [this]
    (let [i ndx]
      (set! ndx (+ 1 i))
      (nth-t2 node i))))

(defn ^counted-iterator new-counted-iterator
  ([^INode node]
   (->counted-iterator node 0 (.get-cnt node)))
  ([^INode node i]
   (->counted-iterator node i (.get-cnt node)))
  )

(defn ^CountedSequence new-counted-seq
  ([node]
   (CountedSequence/create (new-counted-iterator node) identity))
  ([node i]
   (CountedSequence/create (new-counted-iterator node i) identity))
  )

(deftype counted-reverse-iterator
  [node
   ^{:volatile-mutable true int true} ndx]

  Counted
  (count [this] (+ 1 ndx))

  Iterator
  (hasNext [this]
    (>= ndx 0))
  (next [this]
    (let [i ndx]
      (set! ndx (- i 1))
      (nth-t2 node i))))

(defn ^counted-reverse-iterator new-counted-reverse-iterator
  ([^INode node]
   (->counted-reverse-iterator node (- (.get-cnt node) 1)))
  ([node i]
   (->counted-reverse-iterator node i))
  )

(defn ^CountedSequence new-counted-reverse-seq
  ([node]
   (CountedSequence/create (new-counted-reverse-iterator node) identity))
  ([node i]
   (CountedSequence/create (new-counted-reverse-iterator node i) identity))
  )

(defn vector-add [^INode n v i]
  (if (empty-node? n)
    (.newNode n v 1 nil nil 1)
    (let [l (left-node n)
          p (.get-cnt l)]
      (split
        (skew
          (if (<= i p)
            (revise n [:left (vector-add l v i)])
            (revise n [:right (vector-add (right-node n) v (- i p 1))])))))))

(defn vector-set [^INode n v i]
  (if (empty-node? n)
    (.newNode n v 1 nil nil 1)
    (let [l (left-node n)
          p (.get-cnt l)]
      (split
        (skew
          (cond
            (< i p)
            (revise n [:left (vector-set l v i)])
            (> i p)
            (revise n [:right (vector-set (right-node n) v (- i p 1))])
            :else
            (revise n [:t2 v])))))))

(defn ^MapEntry get-entry [^INode this] (.getT2 this))

(defn key-of [^IMapEntry e] (.getKey e))

(defn value-of [^IMapEntry e] (.getValue e))

(defn map-cmpr [this x ^Comparator comparator]
  (.compare comparator x (.getKey (get-entry this))))

(defn map-index-of [this x comparator]
  (if (empty-node? this)
    0
    (let [c (map-cmpr this x comparator)]
      (cond
        (< c 0)
        (map-index-of (left-node this ) x comparator)
        (= c 0)
        (.get-cnt (left-node this))
        :else
        (+ 1
           (.get-cnt (left-node this))
           (map-index-of (right-node this) x comparator))))))

(defn ^counted-iterator new-map-entry-iterator
  ([^INode node x comparator]
   (->counted-iterator node (map-index-of node x comparator) (.get-cnt node)))
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
   (->counted-reverse-iterator node (map-index-of node x comparator)))
  )

(defn ^CountedSequence new-map-entry-reverse-seq
  ([node x comparator]
   (CountedSequence/create (new-map-entry-reverse-iterator node x comparator) identity)))

(defn ^CountedSequence new-map-key-reverse-seq [node]
  (CountedSequence/create (new-counted-reverse-iterator node) key-of))

(defn ^CountedSequence new-map-value-reverse-seq [node]
  (CountedSequence/create (new-counted-reverse-iterator node) value-of))

(defn map-insert [^INode this ^MapEntry t-2 comparator]
  (if (empty-node? this)
    (.newNode this t-2 1 nil nil 1)
    (let [c (map-cmpr this (.getKey t-2) comparator)]
      (split (skew (cond
                     (< c 0)
                     (let [oldl (left-node this)
                           l (map-insert oldl t-2 comparator)]
                       (revise this [:left l]))
                     (> c 0)
                     (let [oldr (right-node this)
                           r (map-insert oldr t-2 comparator)]
                       (revise this [:right r]))
                     :else
                     (if (identical? (.getValue t-2)(.getValue (get-entry this)))
                       this
                       (revise this [:t2 (new MapEntry (.getKey (get-entry this)) (.getValue t-2))]))))))))

(defn map-get-t2 [^INode this x comparator]
  (if (empty-node? this)
    nil
    (let [c (map-cmpr this x comparator)]
      (cond
        (zero? c) (.getT2 this)
        (> c 0) (map-get-t2 (right-node this) x comparator)
        :else (map-get-t2 (left-node this) x comparator)))))

(defn map-del [^INode this x comparator]
  (if (empty-node? this)
    this
    (let [c (map-cmpr this x comparator)]
      (if (and (= c 0) (= 1 (.getLevel this)))
        (right-node this)
        (let [t (cond
                  (> c 0)
                  (revise this [:right (map-del (right-node this) x comparator)])
                  (< c 0)
                  (revise this [:left (map-del (left-node this) x comparator)])
                  :else
                  (let [^MapEntry p (predecessor-t2 this)]
                    (revise this [:t2 p :left (map-del (left-node this) (.getKey p) comparator)])))
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

(declare ->Node)

(defrecord Node [t2 ^int level left right ^int cnt nada]

  INode

  (newNode [this t2 level left right cnt]
    (->Node t2 level left right cnt (empty-node this)))

  (getT2 [this] t2)

  (getLevel [this] level)

  (get-left [this] left)

  (get-right [this] right)

  (get-cnt [this] cnt)

  (get-nada [this] nada)
  )

(defn create-empty-node
  ([] (->Node nil 0 nil nil 0 nil)))

(defn snodev [^INode this]
  (if (empty-node? this)
    ""
    (str (snodev (.get-left this)) " <" (.getT2 this) " " (.getLevel this) "> " (snodev (.get-right this)))))

(defn pnodev [this dsc]
  (println dsc (snodev this)))

(defprotocol flex-vector
  (dropn [this i])
  (addn [this i v]))
