(ns aatree.CountedSequence
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
    :methods [^:static [create [java.util.Iterator clojure.lang.IFn] Object]])
  (:import (java.util Iterator)
           (clojure.lang Counted)
           (aatree CountedSequence)))

(defn -create [^Iterator iter styp]
  (if (.hasNext iter)
    (new aatree.CountedSequence iter styp)
    nil))

(defrecord seq-state [^Iterator iter styp val rst])

(defn iter ^Iterator [seq-state] (:iter seq-state))

(defn -init
  ([^Iterator iter styp]
   (let [s (->seq-state iter styp (atom nil) (atom nil))]
     (reset! (:val s) s)
     (reset! (:rst s) s)
     [[] s]))
  ([meta t s]
   [[meta] s])
  )

(defn -withMeta [^CountedSequence this meta] (new aatree.CountedSequence meta nil (.-state this)))

(defn -first [^CountedSequence this]
  (let [s (.-state this)
        v (:val s)]
    (if (= s @v)
      (swap! v #(if (= s %) (.next (iter s)))))
    (apply (:styp s) [@(:val s)])))

(defn -next [^CountedSequence this]
  (let [s (.-state this)
        r (:rst s)]
    (when (= s @r)
      (-first this)
      (swap! r #(if (= s %) (-create (:iter s) (:styp s)))))
    @(:rst s)))

(defn -count [^CountedSequence this]
  (let [^Counted iter (:iter (.-state this))]
      (.count iter)))
