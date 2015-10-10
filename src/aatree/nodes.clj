(ns aatree.nodes
  (:import (clojure.lang Counted MapEntry IMapEntry)
           (java.util Iterator Comparator)
           (aatree CountedSequence)
           (aatree.CountedSequence XIterator)))

(set! *warn-on-reflection* true)

(definterface INode
  (newNode [t2 ^Long level left right ^Long cnt opts])
  (getT2 [opts])
  (^Long getLevel [opts])
  (getLeft [opts])
  (getRight [opts])
  (^Long getCnt [opts])
  (getNada []))

(deftype noded-state [node opts meta])

(definterface INoded
  (getState [])
  (^aatree.nodes.INode getINode [])
  (getOpts []))

(defn ^noded-state get-state [^INoded this]
  (.getState this))

(defn ^aatree.nodes.INode get-inode [^aatree.nodes.INoded noded]
  (.getINode noded))

(defn get-opts [^aatree.nodes.INoded noded]
  (.getOpts noded))

(defn empty-node? [^INode n]
  (or (nil? n) (identical? n (.getNada n))))

(defn last-t2 [^INode this opts]
  (cond
    (empty-node? this)
    nil
    (empty-node? (.getRight this opts))
    (.getT2 this opts)
    :else
    (recur (.getRight this opts) opts)))

(defn empty-node [^INode this opts]
  (if (empty-node? this)
    this
    (.getNada this)))

(defn ^INode left-node [^INode this opts]
  (if (empty-node? (.getLeft this opts))
    (empty-node this opts)
    (.getLeft this opts)))

(defn ^INode right-node [^INode this opts]
  (if (empty-node? (.getRight this opts))
    (empty-node this opts)
    (.getRight this opts)))

(defn ^Long node-count [^INode this opts]
  (if (empty-node? this)
    0
    (.getCnt this opts)))

(defn revise [^INode this args opts]
  (let [m (apply array-map args)
        t-2 (get m :t2 (.getT2 this opts))
        ^Long lev (get m :level (.getLevel this opts))
        l (get m :left (left-node this opts))
        r (get m :right (right-node this opts))
        ^Long c (+ 1 (node-count l opts) (node-count r opts))]
    (if (and (identical? t-2 (.getT2 this opts))
             (= lev (.getLevel this opts))
             (identical? l (left-node this opts))
             (identical? r (right-node this opts)))
      this
      (.newNode this t-2 lev l r c opts))))

(defn skew
  [^INode this opts]
  (cond
    (empty-node? this)
    this
    (empty-node? (.getLeft this opts))
    this
    (= (.getLevel (left-node this opts) opts) (.getLevel this opts))
    (let [l (.getLeft this opts)]
      (revise l [:right (revise this [:left (right-node l opts)] opts)] opts))
    :else
    this))

(defn split [^INode this opts]
  (cond
    (empty-node? this)
    this
    (or (empty-node? (right-node this opts))
        (empty-node? (right-node (right-node this opts) opts)))
    this
    (= (.getLevel this opts) (.getLevel (right-node (right-node this opts) opts) opts))
    (revise (right-node this opts)
            [:level (+ 1 (.getLevel (right-node this opts) opts))
             :left (revise this [:right (.getLeft (right-node this opts) opts)] opts)]
            opts)
    :else
    this))

(defn predecessor-t2 [this opts]
  (last-t2 (left-node this opts) opts))

(defn decrease-level [^INode this opts]
  (let [should-be (+ 1 (min (.getLevel (left-node this opts) opts)
                            (.getLevel (right-node this opts) opts)))]
    (if (>= should-be (.getLevel this opts))
      this
      (let [rn (right-node this opts)
            rn (if (>= should-be (.getLevel (right-node this opts) opts))
                 rn
                 (revise rn [:level should-be] opts))]
        (revise this [:right rn :level should-be] opts)))))

(defn nth-t2 [^INode this i opts]
  (if (empty-node? this)
    (throw (IndexOutOfBoundsException.))
    (let [l (left-node this opts)
          p (.getCnt l opts)]
      (cond
        (< i p)
        (nth-t2 l i opts)
        (> i p)
        (nth-t2 (right-node this opts) (- i p 1) opts)
        :else
        (.getT2 this opts)))))

(defn deln [^INode this i opts]
  (if (empty-node? this)
    this
    (let [l (left-node this opts)
          p (.getCnt l opts)]
      (if (and (= i p) (= 1 (.getLevel this opts)))
        (right-node this opts)
        (let [t (cond
                  (> i p)
                  (revise this [:right (deln (right-node this opts) (- i p 1) opts)] opts)
                  (< i p)
                  (revise this [:left (deln (left-node this opts) i opts)] opts)
                  :else
                  (let [pre (predecessor-t2 this opts)]
                    (revise this [:t2 pre :left (deln (left-node this opts) (- i 1) opts)] opts)))
              t (decrease-level t opts)
              t (skew t opts)
              t (revise t [:right (skew (right-node t opts) opts)] opts)
              r (right-node t opts)
              t (if (empty-node? r)
                  t
                  (revise t [:right (revise r [:right (skew (right-node r opts) opts)] opts)] opts))
              t (split t opts)
              t (revise t [:right (split (right-node t opts) opts)] opts)]
          t)))))

(deftype counted-iterator
         [node
          ^{:volatile-mutable true Long true} ndx
          ^Long cnt
          opts]

  XIterator
  (count [this index]
    (- cnt index))
  (index [this]
    ndx)
  (bumpIndex [this index]
    (+ 1 index))
  (fetch [this index]
    (nth-t2 node index opts))

  Counted
  (count [this]
    (.count this ndx))

  Iterator
  (hasNext [this]
    (< ndx cnt))
  (next [this]
    (let [i ndx]
      (set! ndx (.bumpIndex this i))
      (.fetch this i))))

(defn ^counted-iterator new-counted-iterator
  ([^INode node opts]
   (->counted-iterator node 0 (.getCnt node opts) opts))
  ([^INode node i opts]
   (->counted-iterator node i (.getCnt node opts) opts)))

(defn ^CountedSequence new-counted-seq
  ([node opts]
   (let [it (new-counted-iterator node  opts)]
     (CountedSequence/create it (.index it) identity)))
  ([node i opts]
   (let [it (new-counted-iterator node i opts)]
     (CountedSequence/create it (.index it) identity))))

(deftype counted-reverse-iterator
         [node
          ^{:volatile-mutable true Long true} ndx
          opts]

  XIterator
  (count [this index]
    (+ 1 index))
  (index [this]
    ndx)
  (bumpIndex [this index]
    (- index 1))
  (fetch [this index]
    (nth-t2 node index opts))

  Counted
  (count [this]
    (.count this ndx))

  Iterator
  (hasNext [this]
    (>= ndx 0))
  (next [this]
    (let [i ndx]
      (set! ndx (.bumpIndex this i))
      (.fetch this i))))

(defn ^counted-reverse-iterator new-counted-reverse-iterator
  ([^INode node opts]
   (->counted-reverse-iterator node (- (.getCnt node opts) 1) opts))
  ([node i opts]
   (->counted-reverse-iterator node i opts)))

(defn ^CountedSequence new-counted-reverse-seq
  ([node opts]
   (let [it (new-counted-reverse-iterator node opts)]
     (CountedSequence/create it (.index it) identity)))
  ([node i opts]
   (let [it (new-counted-reverse-iterator node i opts)]
     (CountedSequence/create it (.index it) identity))))

(defn vector-add [^INode n v i opts]
  (if (empty-node? n)
    (.newNode n v 1 nil nil 1 opts)
    (let [l (left-node n opts)
          p (.getCnt l opts)]
      (split
       (skew
        (if (<= i p)
          (revise n [:left (vector-add l v i opts)] opts)
          (revise n [:right (vector-add (right-node n opts) v (- i p 1) opts)] opts))
        opts)
       opts))))

(defn vector-set [^INode n v i opts]
  (if (empty-node? n)
    (.newNode n v 1 nil nil 1 opts)
    (let [l (left-node n opts)
          p (.getCnt l opts)]
      (split
       (skew
        (cond
          (< i p)
          (revise n [:left (vector-set l v i opts)] opts)
          (> i p)
          (revise n [:right (vector-set (right-node n opts) v (- i p 1) opts)] opts)
          :else
          (revise n [:t2 v] opts))
        opts)
       opts))))

(defn ^MapEntry get-entry [^INode this opts] (.getT2 this opts))

(defn key-of [^IMapEntry e] (.getKey e))

(defn value-of [^IMapEntry e] (.getValue e))

(defn map-cmpr [this x ^Comparator comparator opts]
  (.compare comparator x (.getKey (get-entry this opts))))

(defn resource-cmpr [this x opts] (map-cmpr this x (:comparator opts) opts))

(defn map-index-of [this x opts]
  (if (empty-node? this)
    0
    (let [c (resource-cmpr this x opts)]
      (cond
        (< c 0)
        (map-index-of (left-node this opts) x opts)
        (= c 0)
        (.getCnt (left-node this opts) opts)
        :else
        (+ 1
           (.getCnt (left-node this opts) opts)
           (map-index-of (right-node this opts) x opts))))))

(defn ^counted-iterator new-map-entry-iterator
  ([^INode node x opts]
   (->counted-iterator node (map-index-of node x opts) (.getCnt node opts) opts)))

(defn ^CountedSequence new-map-entry-seq
  ([node x opts]
   (let [it (new-map-entry-iterator node x opts)]
     (CountedSequence/create it (.index it) identity))))

(defn ^CountedSequence new-map-key-seq [node opts]
  (let [it (new-counted-iterator node opts)]
    (CountedSequence/create it (.index it) key-of)))

(defn ^CountedSequence new-map-value-seq [node opts]
  (let [it (new-counted-iterator node opts)]
    (CountedSequence/create it (.index it) value-of)))

(defn ^counted-reverse-iterator new-map-entry-reverse-iterator
  ([node x opts]
   (->counted-reverse-iterator node (map-index-of node x opts) opts)))

(defn ^CountedSequence new-map-entry-reverse-seq
  ([node x opts]
   (let [it (new-map-entry-reverse-iterator node x opts)]
     (CountedSequence/create it (.index it) identity))))

(defn ^CountedSequence new-map-key-reverse-seq [node opts]
  (let [it (new-counted-reverse-iterator node opts)]
    (CountedSequence/create it (.index it) key-of)))

(defn ^CountedSequence new-map-value-reverse-seq [node opts]
  (let [it (new-counted-reverse-iterator node opts)]
    (CountedSequence/create it (.index it) value-of)))

(defn map-insert [^INode this ^MapEntry t-2 opts]
  (if (empty-node? this)
    (.newNode this t-2 1 nil nil 1 opts)
    (let [c (resource-cmpr this (.getKey t-2) opts)]
      (split (skew (cond
                     (< c 0)
                     (let [oldl (left-node this opts)
                           l (map-insert oldl t-2 opts)]
                       (revise this [:left l] opts))
                     (> c 0)
                     (let [oldr (right-node this opts)
                           r (map-insert oldr t-2 opts)]
                       (revise this [:right r] opts))
                     :else
                     (if (identical? (.getValue t-2) (.getValue (get-entry this opts)))
                       this
                       (revise this
                               [:t2 (new MapEntry (.getKey (get-entry this opts)) (.getValue t-2))]
                               opts))) opts) opts))))

(defn map-get-t2 [^INode this x opts]
  (if (empty-node? this)
    nil
    (let [c (resource-cmpr this x opts)]
      (cond
        (zero? c) (.getT2 this opts)
        (> c 0) (map-get-t2 (right-node this opts) x opts)
        :else (map-get-t2 (left-node this opts) x opts)))))

(defn map-del [^INode this x opts]
  (if (empty-node? this)
    this
    (let [c (resource-cmpr this x opts)]
      (if (and (= c 0) (= 1 (.getLevel this opts)))
        (right-node this opts)
        (let [t (cond
                  (> c 0)
                  (revise this [:right (map-del (right-node this opts) x opts)] opts)
                  (< c 0)
                  (revise this [:left (map-del (left-node this opts) x opts)] opts)
                  :else
                  (let [^MapEntry p (predecessor-t2 this opts)]
                    (revise this [:t2 p :left (map-del (left-node this opts) (.getKey p) opts)] opts)))
              t (decrease-level t opts)
              t (skew t opts)
              t (revise t [:right (skew (right-node t opts) opts)] opts)
              r (right-node t opts)
              t (if (empty-node? r)
                  t
                  (revise t [:right (revise r [:right (skew (right-node r opts) opts)] opts)] opts))
              t (split t opts)
              t (revise t [:right (split (right-node t opts) opts)] opts)]
          t)))))

(declare ->Node
         create-empty-node)

(deftype Node [t2 ^Long level left right ^Long cnt]

  INode

  (newNode [this t2 level left right cnt opts]
    (->Node t2 level left right cnt))

  (getT2 [this opts] t2)

  (getLevel [this opts] level)

  (getLeft [this opts] left)

  (getRight [this opts] right)

  (getCnt [this opts] cnt)

  (getNada [this] (create-empty-node)))

(def emptyNode
  (->Node nil 0 nil nil 0))

(defn create-empty-node []
  emptyNode)

(defn snodev [^INode this opts]
  (if (empty-node? this)
    ""
    (str (snodev (.getLeft this opts) opts)
         " <"
         (.getT2 this opts)
         " "
         (.getLevel this opts)
         "> "
         (snodev (.getRight this opts) opts))))

(defn pnodev [this dsc opts]
  (println dsc (snodev this opts)))

(definterface FlexVector
  (dropNode [i])
  (addNode [i v]))
