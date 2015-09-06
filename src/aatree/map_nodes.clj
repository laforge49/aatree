(ns aatree.map-nodes
  (:require [aatree.nodes :refer :all])
  (:import (java.util Comparator)
           (clojure.lang RT)))

(defn create-empty-map-node
  ([] (create-empty-map-node RT/DEFAULT_COMPARATOR))
  ([^Comparator comparator] (->MapNode nil 0 nil nil 0 comparator nil)))
