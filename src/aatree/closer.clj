(ns aatree.closer
  (:require [clojure.tools.logging :as log]))

(set! *warn-on-reflection* true)

(def closer-lock (Object.))

(defn on-close [f opts]
  (locking closer-lock
    (let [fsv (:closer-fsv opts)
          opts (if fsv
                 (do
                   (vreset! fsv (conj @fsv f))
                   opts)
                 (assoc opts :closer-fsv (volatile! (list f))))]
      opts)))

(defn- do-closer [fs opts]
  (when fs
    (try
      ((first fs) opts)
      (catch Exception e
        (log/warn e "exception on close")))
    (recur (next fs) opts)))

(defn do-close [opts]
  (locking closer-lock
    (let [fsv (:closer-fsv opts)]
      (when fsv
        (do-closer @fsv opts)
        (vreset! fsv nil)))))
