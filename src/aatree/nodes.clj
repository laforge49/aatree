(ns aatree.nodes
(:import (clojure.lang IMapEntry)
         (clojure.lang Counted IMapEntry RT MapEntry Indexed)
         (java.util Iterator Comparator)
         (aatree CountedSequence)))


(defprotocol IMapNode
  (emty [this])
  (^int cmpr [this x])
  (right-node [this])
  (left-node [this])
  (new-node [this ^IMapEntry t2 ^int level left right ^int cnt])
  (revise [this args])
  (skew [this])
  (split [this])
  (insert [this ^IMapEntry t-2])
  (^IMapEntry predecessor-t2 [this])
  (^IMapEntry successor-t2 [this])
  (^IMapEntry next-t2 [this x])
  (^IMapEntry prior-t2 [this x])
  (^IMapEntry get-t2 [this x])
  (decrease-level [this])
  (delete [this x])
  (^int index-of [this x])
  (^MapEntry nth-t2 [this ^int i]))

(defn emty? [x]
  (or (nil? x) (zero? (.-level x))))

(defn first-t2 [this]
  (cond
    (emty? this) nil
    (emty? (.-left this)) (.-t2 this)
    :else (recur (.-left this))))

(defn ^IMapEntry last-t2 [this]
  (cond
    (emty? this) nil
    (emty? (.-right this)) (.-t2 this)
    :else (recur (.-right this))))

(defn key-of [^IMapEntry e] (.getKey e))

(defn value-of [^IMapEntry e] (.getValue e))

(deftype map-entry-iterator [node
                             ^{:volatile-mutable true IMapEntry true} lst
                             ^{:volatile-mutable true int true} cnt]
  Iterator
  (hasNext [this]
    (> cnt 0))
  (next [this]
    (if (nil? lst)
      (set! lst (first-t2 node))
      (set! lst (.next-t2 node (.getKey lst))))
    (set! cnt (- cnt 1))
    lst)

  Counted
  (count [this] cnt))

(defn ^map-entry-iterator new-map-entry-iterator
  ([node]
  (->map-entry-iterator node nil (.-cnt node)))
  ([node x]
   (let [ndx (.index_of node x)
         p (.prior-t2 node x)]
     (->map-entry-iterator node p (- (.-cnt node) ndx))))
  )

(defn ^CountedSequence new-map-entry-seq
  ([node]
   (CountedSequence/create (new-map-entry-iterator node) identity))
  ([node x]
   (CountedSequence/create (new-map-entry-iterator node x) identity)))

(defn ^CountedSequence new-map-key-seq [node]
  (CountedSequence/create (new-map-entry-iterator node) key-of))

(defn ^CountedSequence new-map-value-seq [node]
  (CountedSequence/create (new-map-entry-iterator node) value-of))

(deftype map-entry-reverse-iterator [node
                             ^{:volatile-mutable true IMapEntry true} lst
                             ^{:volatile-mutable true int true} cnt]
  Iterator
  (hasNext [this]
    (> cnt 0))
  (next [this]
    (if (nil? lst)
      (set! lst (last-t2 node))
      (set! lst (.prior-t2 node (.getKey lst))))
    (set! cnt (- cnt 1))
    lst)

  Counted
  (count [this] cnt))

(defn ^map-entry-reverse-iterator new-map-entry-reverse-iterator
  ([node]
   (->map-entry-reverse-iterator node nil (.-cnt node)))
  ([node x]
   (let [ndx (.index_of node x)
         n (.next-t2 node x)]
     (->map-entry-reverse-iterator node n (+ 1 ndx)))))

(defn ^CountedSequence new-map-entry-reverse-seq
  ([node]
   (CountedSequence/create (new-map-entry-reverse-iterator node) identity))
  ([node x]
   (CountedSequence/create (new-map-entry-reverse-iterator node x) identity)))

(defn ^CountedSequence new-map-key-reverse-seq [node]
  (CountedSequence/create (new-map-entry-reverse-iterator node) key-of))

(defn ^CountedSequence new-map-value-reverse-seq [node]
  (CountedSequence/create (new-map-entry-reverse-iterator node) value-of))

(defn snodev [this]
  (if (emty? this)
    ""
    (str (snodev (.-left this)) " <" (.-t2 this) " " (.level this) "> " (snodev (.-right this)))))

(defn pnodev [this dsc]
  (println dsc (snodev this)))
