(ns aatree.core
  (:require [aatree.nodes :refer :all])
  (:require [aatree.lazy-nodes :refer :all])
  (:import (aatree AAMap AAVector)
           (aatree.nodes FlexVector)
           (clojure.lang RT)))

(set! *warn-on-reflection* true)

(defn create-aamap
  ([] (new AAMap (create-empty-node) {:comparator RT/DEFAULT_COMPARATOR}))
  ([resources]
   (if (:coparator resources)
   (new AAMap (create-empty-node) resources)
   (new AAMap (create-empty-node) (assoc resources :comparator RT/DEFAULT_COMPARATOR)))))

(def aamap (create-aamap))

(defn create-aavector
  ([] (new AAVector (create-empty-node) {}))
  ([resources] (new AAVector (create-empty-node) resources)))

(def aavector (create-aavector))

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

(defn create-lazy-aavector
  ([] (new AAVector lazy-node {:factory-registry default-factory-registry}))
  ([resources]
   (if (:factory-registry resources)
     (new AAVector lazy-node resources)
     (new AAVector
          lazy-node
          (assoc resources :factory-registry default-factory-registry)))))

(def lazy-aavector (create-lazy-aavector))
