(ns aatree.closer-trait
  (:require [clojure.tools.logging :as log]))

(set! *warn-on-reflection* true)

(defrecord closer-trait [])

(defn on-close [this f]
  (let [fsa (:closer-fsa this)]
    (if fsa
      (do
        (swap! fsa
               (fn [fs]
                 (if fs
                   (conj fs f)
                   (atom (list f)))))
        this)
      (-> (closer-trait.)
          (into this)
          (assoc :closer-fsa (atom (list f)))))))

(defn- do-closer [this fs]
  (when fs
    (try
      ((first fs) this)
      (catch Exception e
        (log/warn e "exception on close")))
    (recur this (next fs))))

(defn do-close [this]
  (let [fsa (:closer-fsa this)]
    (if fsa
      (let [fs @fsa]
        (if fs
          (if (compare-and-set! fsa fs nil)
            (do-closer this fs)
            (recur this)))))))
