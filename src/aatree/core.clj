(ns aatree.core
  (:require [aatree.nodes :refer :all])
  (:require [aatree.lazy-nodes :refer :all])
  (:import (aatree AAMap AAVector)
           (aatree.nodes FlexVector)
           (java.util Comparator)
           (clojure.lang RT)))

(set! *warn-on-reflection* true)

(def aamap (new AAMap (create-empty-node) {:comparator RT/DEFAULT_COMPARATOR}))

(defn create-aamap
  ([] aamap)
  ([resources]
   (if (:coparator resources)
   (new AAMap (create-empty-node) resources)
   (new AAMap (create-empty-node) (assoc resources :comparator RT/DEFAULT_COMPARATOR)))))

(def aavector (new AAVector (create-empty-node) {}))

(defn create-aavector
  ([] aavector)
  ([resources] (new AAVector (create-empty-node) resources)))

(defn addn [^FlexVector vec ndx val]
  (.addNode vec ndx val))

(defn dropn [vec & args]
  (reduce (fn [^FlexVector v i] (.dropNode v i)) vec args))

(def lazy-aamap (new AAMap lazy-node {:comparator RT/DEFAULT_COMPARATOR
                                      :factory-registry default-factory-registry}))

(defn create-lazy-aamap
  ([] lazy-aamap)
  ([resources]
   (let [r resources
         r (if (:comparator r)
             r
             (assoc r :comparator RT/DEFAULT_COMPARATOR))
         r (if (:factory-registry r)
             r
             (assoc r :factory-registry default-factory-registry))]
     (new AAMap lazy-node r))))

(def lazy-aavector (new AAVector lazy-node {:factory-registry default-factory-registry}))

(defn create-lazy-aavector
  ([] lazy-aavector)
  ([resources]
   (if (:factory-registry resources)
     (new AAVector lazy-node resources)
     (new AAVector
          lazy-node
          (assoc resources :factory-registry default-factory-registry)))))
