(ns aatree.core
  (:require [aatree.nodes :refer :all])
  (:require [aatree.lazy-nodes :refer :all])
  (:import (aatree AAMap AAVector)
           (aatree.nodes FlexVector)
           (clojure.lang RT)))

(set! *warn-on-reflection* true)

(def emptyAAMap
  (new AAMap emptyNode {:comparator RT/DEFAULT_COMPARATOR}))

(defn create-aamap
  ([] emptyAAMap)
  ([resources]
   (if (:coparator resources)
     (new AAMap emptyNode resources)
     (new AAMap emptyNode (assoc resources :comparator RT/DEFAULT_COMPARATOR)))))

(def emptyAAVector
  (new AAVector emptyNode {}))

(defn create-aavector
  ([] emptyAAVector)
  ([resources] (new AAVector emptyNode resources)))

(defn addn [^FlexVector vec ndx val]
  (.addNode vec ndx val))

(defn dropn [vec & args]
  (reduce (fn [^FlexVector v i] (.dropNode v i)) vec args))

(def emptyLazyAAMap
  (new AAMap emptyLazyNode {:comparator RT/DEFAULT_COMPARATOR
                            :factory-registry default-factory-registry}))

(defn create-lazy-aamap
  ([] emptyLazyAAMap)
  ([resources]
   (let [r resources
         r (if (:comparator r)
             r
             (assoc r :comparator RT/DEFAULT_COMPARATOR))
         r (if (:factory-registry r)
             r
             (assoc r :factory-registry default-factory-registry))]
     (new AAMap emptyLazyNode r))))

(def emptyLazyAAVector
  (new AAVector emptyLazyNode {:factory-registry default-factory-registry}))

(defn create-lazy-aavector
  ([] emptyLazyAAVector)
  ([resources]
   (if (:factory-registry resources)
     (new AAVector emptyLazyNode resources)
     (new AAVector
          emptyLazyNode
          (assoc resources :factory-registry default-factory-registry)))))

(defn load-aavector
  ([buffer]
   (load-aavector buffer {}))
  ([buffer resources]
   (if (:factory-registry resources)
     (new AAVector (node-read buffer resources) resources)
     (let [r (assoc resources :factory-registry default-factory-registry)]
       (new AAVector (node-read buffer r) r)))))

(defn load-aamap
  ([buffer]
   (load-aamap buffer {}))
  ([buffer resources]
   (let [r resources
         r (if (:comparator r)
             r
             (assoc r :comparator RT/DEFAULT_COMPARATOR))
         r (if (:factory-registry r)
             r
             (assoc r :factory-registry default-factory-registry))]
     (new AAMap (node-read buffer r) r))))

(defn lazy-byte-length [noded]
  (node-byte-length (get-inode noded) (get-resources noded)))

(defn lazy-write [noded buffer]
  (node-write (get-inode noded) buffer (get-resources noded)))
