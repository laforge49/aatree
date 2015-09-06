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

(defn empty-node? [x]
  (or (nil? x) (zero? (.-level x))))

(defn first-t2 [this]
  (cond
    (empty-node? this) nil
    (empty-node? (.-left this)) (.-t2 this)
    :else (recur (.-left this))))

(defn ^IMapEntry last-t2 [this]
  (cond
    (empty-node? this) nil
    (empty-node? (.-right this)) (.-t2 this)
    :else (recur (.-right this))))

(deftype counted-iterator
  [node
   ^{:volatile-mutable true int true} ndx
   ^int cnt]

  Counted
  (count [this] cnt)

  Iterator
  (hasNext [this]
    (< ndx cnt))
  (next [this]
    (let [i ndx]
      (set! ndx (+ 1 i))
      (.nth-t2 node i))))

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

(defn snodev [this]
  (if (empty-node? this)
    ""
    (str (snodev (.-left this)) " <" (.-t2 this) " " (.level this) "> " (snodev (.-right this)))))

(defn pnodev [this dsc]
  (println dsc (snodev this)))
