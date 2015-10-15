(ns aatree.core
  (:require [aatree.nodes :refer :all])
  (:require [aatree.lazy-nodes :refer :all])
  (:import (aatree AAMap AAVector AASet)
           (aatree.nodes FlexVector)
           (clojure.lang RT)))

(set! *warn-on-reflection* true)

(def ^{:deprecated "0.4.0"} emptyAAMap
  (new AAMap emptyNode {:comparator RT/DEFAULT_COMPARATOR}))

(defn ^{:deprecated "0.4.0"} create-aamap
  ([] emptyAAMap)
  ([opts]
   (if (:coparator opts)
     (new AAMap emptyNode opts)
     (new AAMap emptyNode (assoc opts :comparator RT/DEFAULT_COMPARATOR)))))

(def ^{:deprecated "0.4.0"} emptyAAVector
  (new AAVector emptyNode {}))

(defn ^{:deprecated "0.4.0"} create-aavector
  ([] emptyAAVector)
  ([opts] (new AAVector emptyNode opts)))

(defn addn [^FlexVector vec ndx val]
  (.addNode vec ndx val))

(defn dropn [vec & args]
  (reduce (fn [^FlexVector v i] (.dropNode v i)) vec args))

(def ^{:deprecated "0.4.0"} emptyLazyAAMap
  (new AAMap emptyLazyNode (map-opts {:comparator RT/DEFAULT_COMPARATOR
                                      :factory-registry default-factory-registry})))

(defn ^{:deprecated "0.4.0"} create-lazy-aamap
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

(def ^{:deprecated "0.4.0"} emptyLazyAAVector
  (new AAVector emptyLazyNode (vector-opts {:factory-registry default-factory-registry})))

(defn ^{:deprecated "0.4.0"} create-lazy-aavector
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
     (let [r (vector-opts opts)]
       (new AAVector (node-read buffer r) r))
     (let [r (assoc opts :factory-registry default-factory-registry)
           r (vector-opts r)]
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
             (assoc r :factory-registry default-factory-registry))
         r (map-opts r)]
     (new AAMap (node-read buffer r) r))))

(defn lazy-byte-length [noded]
  (node-byte-length (get-inode noded) (get-opts noded)))

(defn lazy-write [noded buffer]
  (node-write (get-inode noded) buffer (get-opts noded)))

;new in 0.3.3

(def ^{:deprecated "0.4.0"} emptyAASet
  (new AASet emptyAAMap))

(defn ^{:deprecated "0.4.0"} create-aaset
  ([] emptyAASet)
  ([opts]
   (let [mpl
         (if (:coparator opts)
           (new AAMap emptyNode opts)
           (new AAMap emptyNode (assoc opts :comparator RT/DEFAULT_COMPARATOR)))]
     (new AASet mpl))))

(def ^{:deprecated "0.4.0"} emptyLazyAASet
  (new AASet
       (new AAMap emptyLazyNode (set-opts {:comparator RT/DEFAULT_COMPARATOR
                                           :factory-registry default-factory-registry}))))

(defn ^{:deprecated "0.4.0"} create-lazy-aaset
  ([] emptyLazyAASet)
  ([opts]
   (let [r opts
         r (if (:comparator r)
             r
             (assoc r :comparator RT/DEFAULT_COMPARATOR))
         r (if (:factory-registry r)
             r
             (assoc r :factory-registry default-factory-registry))
         r (set-opts r)]
     (new AASet
          (new AAMap emptyLazyNode r)))))

(defn load-aaset
  ([buffer]
   (load-aaset buffer {}))
  ([buffer opts]
   (let [r opts
         r (if (:comparator r)
             r
             (assoc r :comparator RT/DEFAULT_COMPARATOR))
         r (if (:factory-registry r)
             r
             (assoc r :factory-registry default-factory-registry))
         r (set-opts r)]
     (new AASet
          (new AAMap (node-read buffer r) r)))))

;new in 0.4.0

(defn standard-opts
  ([] (standard-opts {}))
  ([opts]
   (-> opts
       (assoc :new-sorted-map
              (fn [o]
                (let [c (:comparator o)]
                  (if c
                    (sorted-map-by c)
                    (sorted-map)))))
       (assoc :new-vector
              (fn [o] []))
       (assoc :new-sorted-set
              (fn [o]
                (let [c (:comparator o)]
                  (if c
                    (sorted-set-by c)
                    (sorted-set))))))))

(defn basic-opts
  ([] (basic-opts {}))
  ([opts]
   (-> opts
       (assoc :new-sorted-map
              (fn [o]
                (if (:coparator opts)
                  (new AAMap emptyNode o)
                  (new AAMap
                       emptyNode
                       (assoc o :comparator RT/DEFAULT_COMPARATOR)))))
       (assoc :new-vector
              (fn [o] (new AAVector emptyNode o)))
       (assoc :new-sorted-set
              (fn [o]
                (let [mpl
                      (if (:coparator o)
                        (new AAMap emptyNode o)
                        (new AAMap emptyNode (assoc
                                              o
                                              :comparator
                                              RT/DEFAULT_COMPARATOR)))]
                  (new AASet mpl)))))))

(defn lazy-opts
  ([] (lazy-opts {}))
  ([opts]
   (-> opts
       (assoc :new-sorted-map
              (fn [r]
                (let [r (if (:comparator r)
                          r
                          (assoc r :comparator RT/DEFAULT_COMPARATOR))
                      r (if (:factory-registry r)
                          r
                          (assoc r :factory-registry default-factory-registry))
                      r (map-opts r)]
                  (new AAMap emptyLazyNode r))))
       (assoc :new-vector
              (fn [o]
                (if (:factory-registry o)
                  (new AAVector emptyLazyNode (vector-opts o))
                  (new AAVector
                       emptyLazyNode
                       (vector-opts (assoc o :factory-registry default-factory-registry))))))
       (assoc :new-sorted-set
              (fn [o]
                (let [r opts
                      r (if (:comparator r)
                          r
                          (assoc r :comparator RT/DEFAULT_COMPARATOR))
                      r (if (:factory-registry r)
                          r
                          (assoc r :factory-registry default-factory-registry))
                      r (set-opts r)]
                  (new AASet
                       (new AAMap emptyLazyNode r))))))))

(defn new-sorted-map [opts]
  ((:new-sorted-map opts) opts))

(defn new-vector [opts]
  ((:new-vector opts) opts))

(defn new-sorted-set [opts]
  ((:new-sorted-set opts) opts))
