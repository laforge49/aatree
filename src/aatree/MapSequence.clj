(ns aatree.MapSequence
  (:gen-class
    :main false
    :extends clojure.lang.ASeq
    :implements [clojure.lang.Counted]
    :constructors {[java.util.Iterator clojure.lang.IFn]
                   []
                   [clojure.lang.IPersistentMap clojure.lang.IFn Object]
                   [clojure.lang.IPersistentMap]}
    :init init
    :state state
    :methods [^:static [create [java.util.Iterator clojure.lang.IFn] Object]]
    :exposes-methods {count superCount})
  )

(defn -create [iter styp]
  (if (.hasNext iter)
    (new aatree.MapSequence iter styp)
    nil))

(deftype seq-state [iter styp val rst])

(defn -init
  ([iter styp]
   (let [s (->seq-state iter styp (atom nil) (atom nil))]
     (reset! (.-val s) s)
     (reset! (.-rst s) s)
     [[] s]))
  ([meta t s]
   [[meta] s])
  )

(defn -withMeta [this meta] (new aatree.MapSequence meta nil (.-state this)))

(defn -first [this]
  (let [s (.-state this)
        v (.-val s)]
    (if (= s @v)
      (swap! v #(if (= s %) (.next (.-iter s)))))
    (apply (.-styp (.state this)) [@(.-val s)])))

(defn -next [this]
  (let [s (.-state this)
        r (.-rst s)]
    (when (= s @r)
      (-first this)
      (swap! r #(if (= s %) (-create (.-iter s) (.-styp (.-state this))))))
    @(.-rst s)))

(defn -count [this]
  (let [iter (.iter (.-state this))]
    (if (counted? iter)
      (.count iter)
      (.superCount this))))