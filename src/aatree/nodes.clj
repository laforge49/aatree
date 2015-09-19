(ns aatree.nodes
  (:import (clojure.lang Counted MapEntry IMapEntry)
           (java.util Iterator Comparator)
           (aatree CountedSequence)))

(set! *warn-on-reflection* true)

(definterface INode
  (newNode [t2 ^int level left right ^int cnt resources])
  (getT2 [resources])
  (getLevel [resources])
  (getLeft [resources])
  (getRight [resources])
  (getCnt [resources])
  (getNada [resources]))

(defn empty-node? [^INode n resources]
  (or (nil? n) (zero? (.getLevel n resources))))

(defn last-t2 [^INode this resources]
  (cond
    (empty-node? this resources) nil
    (empty-node? (.getRight this resources) resources) (.getT2 this resources)
    :else (recur (.getRight this resources) resources)))

(defn empty-node [^INode this resources]
  (if (empty-node? this resources)
    this
    (.getNada this resources)))

(defn ^INode left-node [^INode this resources]
  (if (empty-node? (.getLeft this resources) resources)
    (empty-node this resources)
    (.getLeft this resources)))

(defn ^INode right-node [^INode this resources]
  (if (empty-node? (.getRight this resources) resources)
    (empty-node this resources)
    (.getRight this resources)))

(defn node-count [^INode this resources]
  (if (empty-node? this resources)
    0
    (.getCnt this resources)))

(defn revise [^INode this args resources]
  (let [m (apply array-map args)
        t-2 (get m :t2 (.getT2 this resources))
        lev (get m :level (.getLevel this resources))
        l (get m :left (left-node this resources))
        r (get m :right (right-node this resources))
        c (+ 1 (node-count l resources) (node-count r resources))]
    (if (and (identical? t-2 (.getT2 this resources))
             (= lev (.getLevel this resources))
             (identical? l (left-node this resources))
             (identical? r (right-node this resources)))
      this
      (.newNode this t-2 lev l r c resources))))

(defn skew
  [^INode this resources]
  (cond
    (empty-node? this resources)
    this
    (empty-node? (.getLeft this resources) resources)
    this
    (= (.getLevel (left-node this resources) resources) (.getLevel this resources))
    (let [l (.getLeft this resources)]
      (revise l [:right (revise this [:left (right-node l resources)] resources)] resources))
    :else
    this))

(defn split [^INode this resources]
  (cond
    (empty-node? this resources)
    this
    (or (empty-node? (right-node this resources) resources)
        (empty-node? (right-node (right-node this resources) resources) resources))
    this
    (= (.getLevel this resources) (.getLevel (right-node (right-node this resources) resources) resources))
    (revise (right-node this resources)
            [:level (+ 1 (.getLevel (right-node this resources) resources))
             :left (revise this [:right (.getLeft (right-node this resources) resources)] resources)]
            resources)
    :else
    this))

(defn predecessor-t2 [this resources]
  (last-t2 (left-node this resources) resources))

(defn decrease-level [^INode this resources]
  (let [should-be (+ 1 (min (.getLevel (left-node this resources) resources)
                            (.getLevel (right-node this resources) resources)))]
    (if (>= should-be (.getLevel this resources))
      this
      (let [rn (right-node this resources)
            rn (if (>= should-be (.getLevel (right-node this resources) resources))
                 rn
                 (revise rn [:level should-be] resources))]
        (revise this [:right rn :level should-be] resources)))))

(defn nth-t2 [^INode this i resources]
  (if (empty-node? this resources)
    (throw (IndexOutOfBoundsException.))
    (let [l (left-node this resources)
          p (.getCnt l resources)]
      (cond
        (< i p)
        (nth-t2 l i resources)
        (> i p)
        (nth-t2 (right-node this resources) (- i p 1) resources)
        :else
        (.getT2 this resources)))))

(defn deln [^INode this i resources]
  (if (empty-node? this resources)
    this
    (let [l (left-node this resources)
          p (.getCnt l resources)]
      (if (and (= i p) (= 1 (.getLevel this resources)))
        (right-node this resources)
        (let [t (cond
                  (> i p)
                  (revise this [:right (deln (right-node this resources) (- i p 1) resources)] resources)
                  (< i p)
                  (revise this [:left (deln (left-node this resources) i resources)] resources)
                  :else
                  (let [pre (predecessor-t2 this resources)]
                    (revise this [:t2 pre :left (deln (left-node this resources) (- i 1) resources)] resources)))
              t (decrease-level t resources)
              t (skew t resources)
              t (revise t [:right (skew (right-node t resources) resources)] resources)
              r (right-node t resources)
              t (if (empty-node? r resources)
                  t
                  (revise t [:right (revise r [:right (skew (right-node r resources) resources)] resources)] resources))
              t (split t resources)
              t (revise t [:right (split (right-node t resources) resources)] resources)]
          t)))))

(deftype counted-iterator
  [node
   ^{:volatile-mutable true int true} ndx
   ^int cnt
   resources]

  Counted
  (count [this] (- cnt ndx))

  Iterator
  (hasNext [this]
    (< ndx cnt))
  (next [this]
    (let [i ndx]
      (set! ndx (+ 1 i))
      (nth-t2 node i resources))))

(defn ^counted-iterator new-counted-iterator
  ([^INode node resources]
   (->counted-iterator node 0 (.getCnt node resources) resources))
  ([^INode node i resources]
   (->counted-iterator node i (.getCnt node resources) resources)))

(defn ^CountedSequence new-counted-seq
  ([node resources]
   (CountedSequence/create (new-counted-iterator node  resources) identity))
  ([node i resources]
   (CountedSequence/create (new-counted-iterator node i resources) identity)))

(deftype counted-reverse-iterator
  [node
   ^{:volatile-mutable true int true} ndx
   resources]

  Counted
  (count [this] (+ 1 ndx))

  Iterator
  (hasNext [this]
    (>= ndx 0))
  (next [this]
    (let [i ndx]
      (set! ndx (- i 1))
      (nth-t2 node i resources))))

(defn ^counted-reverse-iterator new-counted-reverse-iterator
  ([^INode node resources]
   (->counted-reverse-iterator node (- (.getCnt node resources) 1) resources))
  ([node i resources]
   (->counted-reverse-iterator node i resources)))

(defn ^CountedSequence new-counted-reverse-seq
  ([node resources]
   (CountedSequence/create (new-counted-reverse-iterator node resources) identity))
  ([node i resources]
   (CountedSequence/create (new-counted-reverse-iterator node i resources) identity)))

(defn vector-add [^INode n v i resources]
  (if (empty-node? n resources)
    (.newNode n v 1 nil nil 1 resources)
    (let [l (left-node n resources)
          p (.getCnt l resources)]
      (split
        (skew
          (if (<= i p)
            (revise n [:left (vector-add l v i resources)] resources)
            (revise n [:right (vector-add (right-node n resources) v (- i p 1) resources)] resources))
          resources)
        resources))))

(defn vector-set [^INode n v i resources]
  (if (empty-node? n resources)
    (.newNode n v 1 nil nil 1 resources)
    (let [l (left-node n resources)
          p (.getCnt l resources)]
      (split
        (skew
          (cond
            (< i p)
            (revise n [:left (vector-set l v i resources)] resources)
            (> i p)
            (revise n [:right (vector-set (right-node n resources) v (- i p 1) resources)] resources)
            :else
            (revise n [:t2 v] resources))
          resources)
        resources))))

(defn ^MapEntry get-entry [^INode this resources] (.getT2 this resources))

(defn key-of [^IMapEntry e] (.getKey e))

(defn value-of [^IMapEntry e] (.getValue e))

(defn map-cmpr [this x ^Comparator comparator resources]
  (.compare comparator x (.getKey (get-entry this resources))))

(defn resource-cmpr [this x resources] (map-cmpr this x (:comparator resources) resources))

(defn map-index-of [this x resources]
  (if (empty-node? this resources)
    0
    (let [c (resource-cmpr this x resources)]
      (cond
        (< c 0)
        (map-index-of (left-node this resources) x resources)
        (= c 0)
        (.getCnt (left-node this resources) resources)
        :else
        (+ 1
           (.getCnt (left-node this resources) resources)
           (map-index-of (right-node this resources) x resources))))))

(defn ^counted-iterator new-map-entry-iterator
  ([^INode node x resources]
   (->counted-iterator node (map-index-of node x resources) (.getCnt node resources) resources)))

(defn ^CountedSequence new-map-entry-seq
  ([node x resources]
   (CountedSequence/create (new-map-entry-iterator node x resources) identity)))

(defn ^CountedSequence new-map-key-seq [node resources]
  (CountedSequence/create (new-counted-iterator node resources) key-of))

(defn ^CountedSequence new-map-value-seq [node resources]
  (CountedSequence/create (new-counted-iterator node resources) value-of))

(defn ^counted-reverse-iterator new-map-entry-reverse-iterator
  ([node x resources]
   (->counted-reverse-iterator node (map-index-of node x resources) resources)))

(defn ^CountedSequence new-map-entry-reverse-seq
  ([node x resources]
   (CountedSequence/create (new-map-entry-reverse-iterator node x resources) identity)))

(defn ^CountedSequence new-map-key-reverse-seq [node resources]
  (CountedSequence/create (new-counted-reverse-iterator node resources) key-of))

(defn ^CountedSequence new-map-value-reverse-seq [node resources]
  (CountedSequence/create (new-counted-reverse-iterator node resources) value-of))

(defn map-insert [^INode this ^MapEntry t-2 resources]
  (if (empty-node? this resources)
    (.newNode this t-2 1 nil nil 1 resources)
    (let [c (resource-cmpr this (.getKey t-2) resources)]
      (split (skew (cond
                     (< c 0)
                     (let [oldl (left-node this resources)
                           l (map-insert oldl t-2 resources)]
                       (revise this [:left l] resources))
                     (> c 0)
                     (let [oldr (right-node this resources)
                           r (map-insert oldr t-2 resources)]
                       (revise this [:right r] resources))
                     :else
                     (if (identical? (.getValue t-2) (.getValue (get-entry this resources)))
                       this
                       (revise this
                               [:t2 (new MapEntry (.getKey (get-entry this resources)) (.getValue t-2))]
                               resources))) resources) resources))))

(defn map-get-t2 [^INode this x resources]
  (if (empty-node? this resources)
    nil
    (let [c (resource-cmpr this x resources)]
      (cond
        (zero? c) (.getT2 this resources)
        (> c 0) (map-get-t2 (right-node this resources) x resources)
        :else (map-get-t2 (left-node this resources) x resources)))))

(defn map-del [^INode this x resources]
  (if (empty-node? this resources)
    this
    (let [c (resource-cmpr this x resources)]
      (if (and (= c 0) (= 1 (.getLevel this resources)))
        (right-node this resources)
        (let [t (cond
                  (> c 0)
                  (revise this [:right (map-del (right-node this resources) x resources)] resources)
                  (< c 0)
                  (revise this [:left (map-del (left-node this resources) x resources)] resources)
                  :else
                  (let [^MapEntry p (predecessor-t2 this resources)]
                    (revise this [:t2 p :left (map-del (left-node this resources) (.getKey p) resources)] resources)))
              t (decrease-level t resources)
              t (skew t resources)
              t (revise t [:right (skew (right-node t resources) resources)] resources)
              r (right-node t resources)
              t (if (empty-node? r resources)
                  t
                  (revise t [:right (revise r [:right (skew (right-node r resources) resources)] resources)] resources))
              t (split t resources)
              t (revise t [:right (split (right-node t resources) resources)] resources)]
          t)))))

(declare ->Node
         create-empty-node)

(deftype Node [t2 ^int level left right ^int cnt]

  INode

  (newNode [this t2 level left right cnt resources]
    (->Node t2 level left right cnt))

  (getT2 [this resources] t2)

  (getLevel [this resources] level)

  (getLeft [this resources] left)

  (getRight [this resources] right)

  (getCnt [this resources] cnt)

  (getNada [this resources] (create-empty-node)))

(def emptyNode
  (->Node nil 0 nil nil 0))

(defn create-empty-node []
  emptyNode)

(defn snodev [^INode this resources]
  (if (empty-node? this resources)
    ""
    (str (snodev (.getLeft this resources) resources)
         " <"
         (.getT2 this resources)
         " "
         (.getLevel this resources)
         "> "
         (snodev (.getRight this resources) resources))))

(defn pnodev [this dsc resources]
  (println dsc (snodev this resources)))

(definterface FlexVector
  (dropNode [i])
  (addNode [i v]))
