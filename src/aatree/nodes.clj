(ns aatree.nodes
(:import (clojure.lang IMapEntry)
         (clojure.lang Counted IMapEntry RT)
         (java.util Iterator Comparator)
         (aatree MapSequence)))


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
  (delete [this x]))

(defn emty? [x]
  (or (nil? x) (zero? (.-level x))))

(declare ->MapNode)

(defn emty-node
  ([] (emty-node RT/DEFAULT_COMPARATOR))
  ([^Comparator comparator] (->MapNode nil 0 nil nil 0 comparator nil)))

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

(defn ^map-entry-iterator new-map-entry-iterator [node]
  (->map-entry-iterator node nil (.-cnt node)))

(defn ^MapSequence new-map-entry-seq [node]
  (MapSequence/create (new-map-entry-iterator node) identity))

(defn ^MapSequence new-map-key-seq [node]
  (MapSequence/create (new-map-entry-iterator node) key-of))

(defn ^MapSequence new-map-value-seq [node]
  (MapSequence/create (new-map-entry-iterator node) value-of))

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

(defn ^map-entry-reverse-iterator new-map-entry-reverse-iterator [node]
  (->map-entry-reverse-iterator node nil (.-cnt node)))

(defn ^MapSequence new-map-entry-reverse-seq [node]
  (MapSequence/create (new-map-entry-reverse-iterator node) identity))

(defn ^MapSequence new-map-key-reverse-seq [node]
  (MapSequence/create (new-map-entry-reverse-iterator node) key-of))

(defn ^MapSequence new-map-value-reverse-seq [node]
  (MapSequence/create (new-map-entry-reverse-iterator node) value-of))

(defn snodev [this]
  (if (emty? this)
    ""
    (str (snodev (.-left this)) " <" (.-t2 this) " " (.level this) "> " (snodev (.-right this)))))

(defn pnodev [this dsc]
  (println dsc (snodev this)))

(deftype MapNode [^IMapEntry t2 ^int level left right ^int cnt ^Comparator comparator nada]

  Counted

  (count [this]
    (if (emty? this)
      0
      cnt))

  IMapNode

  (emty [this]
    (if (emty? this)
      this
      nada))

  (cmpr [this x]
    (.compare comparator x (.getKey t2)))

  (right-node [this]
    (if (emty? right)
      (.emty this)
      right))

  (left-node [this]
    (if (emty? (.-left this))
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
      (emty? this)
      this
      (emty? left)
      this
      (= (.-level left) level)
      (let [l left]
        (.revise l [:right (.revise this [:left (.right-node l)])]))
      :else
      this))

  (split [this]
    (cond
      (emty? this)
      this
      (or (emty? right) (emty? (.-right right)))
      this
      (= level (.-level (.-right right)))
      (.revise right
               [:level (+ 1 (.-level right))
                :left (.revise this [:right (.-left right)])])
      :else
      this))

  (insert [this t-2]
    (if (emty? this)
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
                         (.revise this [:t2 t-2])))))))

  (predecessor-t2 [this]
    (last-t2 (.left-node this)))

  (successor-t2 [this]
    (first-t2 (.right-node this)))

  (next-t2 [this x]
    (if (emty? this)
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
    (if (emty? this)
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
    (if (emty? this)
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
    (if (emty? this)
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
                t (if (emty? r)
                    t
                    (.revise t [:right (.revise r [:right (.skew (.right-node r))])]))
                t (.split t)
                t (.revise t [:right (.split (.right-node t))])]
            t)))))
  )
