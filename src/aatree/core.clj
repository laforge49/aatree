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
  ([opts]
   (if (:coparator opts)
     (new AAMap emptyNode opts)
     (new AAMap emptyNode (assoc opts :comparator RT/DEFAULT_COMPARATOR)))))

(def emptyAAVector
  (new AAVector emptyNode {}))

(defn create-aavector
  ([] emptyAAVector)
  ([opts] (new AAVector emptyNode opts)))

(defn addn [^FlexVector vec ndx val]
  (.addNode vec ndx val))

(defn dropn [vec & args]
  (reduce (fn [^FlexVector v i] (.dropNode v i)) vec args))

(def emptyLazyAAMap
  (new AAMap emptyLazyNode (map-opts {:comparator RT/DEFAULT_COMPARATOR
                                      :factory-registry default-factory-registry})))

(defn create-lazy-aamap
  ([] emptyLazyAAMap)
  ([opts]
   (let [r opts
         r (if (:comparator r)
             r
             (assoc r :comparator RT/DEFAULT_COMPARATOR))
         r (if (:factory-registry r)
             r
             (assoc r :factory-registry default-factory-registry))
         r (map-opts r)]
     (new AAMap emptyLazyNode r))))

(def emptyLazyAAVector
  (new AAVector emptyLazyNode (vector-opts {:factory-registry default-factory-registry})))

(defn create-lazy-aavector
  ([] emptyLazyAAVector)
  ([opts]
   (if (:factory-registry opts)
     (new AAVector emptyLazyNode (vector-opts opts))
     (new AAVector
          emptyLazyNode
          (vector-opts (assoc opts :factory-registry default-factory-registry))))))

(defn load-aavector
  ([buffer]
   (load-aavector buffer {}))
  ([buffer opts]
   (if (:factory-registry opts)
     (new AAVector (node-read buffer opts) opts)
     (let [r (assoc opts :factory-registry default-factory-registry)]
       (new AAVector (node-read buffer r) r)))))

(defn load-aamap
  ([buffer]
   (load-aamap buffer {}))
  ([buffer opts]
   (let [r opts
         r (if (:comparator r)
             r
             (assoc r :comparator RT/DEFAULT_COMPARATOR))
         r (if (:factory-registry r)
             r
             (assoc r :factory-registry default-factory-registry))]
     (new AAMap (node-read buffer r) r))))

(defn lazy-byte-length [noded]
  (node-byte-length (get-inode noded) (get-opts noded)))

(defn lazy-write [noded buffer]
  (node-write (get-inode noded) buffer (get-opts noded)))
