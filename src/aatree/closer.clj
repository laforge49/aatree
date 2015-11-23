(ns aatree.closer
  (:require [clojure.tools.logging :as log]))

(set! *warn-on-reflection* true)

(defn on-close [f opts]
  (let [fsa (:closer-fsa opts)]
    (if fsa
      (do
        (swap! fsa
               (fn [fs]
                 (if fs
                   (conj fs f)
                   (atom (list f)))))
        opts)
      (assoc opts :closer-fsa (atom (list f))))))

(defn- do-closer [fs opts]
  (when fs
    (try
      ((first fs) opts)
      (catch Exception e
        (log/warn e "exception on close")))
    (recur (next fs) opts)))

(defn do-close [opts]
  (let [fsa (:closer-fsa opts)]
    (if fsa
      (swap! fsa
             (fn [fs]
               (do-closer fs opts)
               nil)))))
