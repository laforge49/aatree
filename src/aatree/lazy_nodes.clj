(ns aatree.lazy-nodes
  (:require [aatree.nodes :refer :all])
  (:import (java.nio ByteBuffer)
           (aatree.nodes Node IFactory WrapperNode)
           (clojure.lang RT)
           (aatree AAVector AAMap AASet)))

(set! *warn-on-reflection* true)

(declare ->LazyNode
         ^aatree.nodes.INode get-lazy-data
         create-lazy-empty-node
         lazy-byte-length
         lazy-write)

(deftype LazyNode [data-atom sval-atom blen-atom buffer-atom factory]

  aatree.nodes.INode

  (newNode [this t2 level left right cnt opts]
    (let [d (->Node t2 level left right cnt)
          f (factory-for-instance t2 opts)]
      (->LazyNode (atom d) (atom nil) (atom nil) (atom nil) f)))

  (getT2 [this opts] (.getT2 (get-lazy-data this opts) opts))

  (^Long getLevel [this opts] (.getLevel (get-lazy-data this opts) opts))

  (getLeft [this opts] (.getLeft (get-lazy-data this opts) opts))

  (getRight [this opts] (.getRight (get-lazy-data this opts) opts))

  (^Long getCnt [this opts] (.getCnt (get-lazy-data this opts) opts))

  (getNada [this] (create-lazy-empty-node))

  WrapperNode

  (svalAtom [this] (.-sval-atom this))

  (blenAtom [this] (.-blen-atom this))

  (bufferAtom [this] (.-buffer-atom this))

  (factory [this] (.-factory this))

  (nodeByteLength [this opts] (lazy-byte-length this opts))

  (nodeWrite [this buffer opts] (lazy-write this buffer opts)))

(defn- get-data-atom [^LazyNode this] (.-data-atom this))

(defn lazy-byte-length [^LazyNode lazy-node opts]
  (if (empty-node? lazy-node)
    1
    (let [a (.blenAtom lazy-node)
          blen @a]
      (if (nil? blen)
        (let [^ByteBuffer bb @(.bufferAtom lazy-node)
              blen (if bb
                     (.limit bb)
                     (+ 1 ;node id
                        4 ;byte length - 5
                        (lazy-byte-length (left-node lazy-node opts) opts) ;left node
                        4 ;level
                        4 ;cnt
                        (.valueLength (get-factory lazy-node) lazy-node opts) ;t2
                        (lazy-byte-length (right-node lazy-node opts) opts)))] ;right node
          (compare-and-set! a nil blen)))
      @a)))

(defn lazy-write [^LazyNode lazy-node ^ByteBuffer buffer opts]
  (let [^IFactory f (.factory lazy-node)
        ^ByteBuffer old-bb (get-buffer lazy-node)]
    (if old-bb
      (let [new-bb (.duplicate old-bb)
            lim (.limit new-bb)
            ba (byte-array lim)]
        (.get new-bb ba)
        (.put buffer ba))
      (let [new-bb (.slice buffer)]
        (if (= (byte \n) (.factoryId f))
          (.put buffer (byte (.factoryId f)))
          (do
            (.put buffer (byte (.factoryId f)))
            (.putInt buffer (- (lazy-byte-length lazy-node opts) 5))
            (lazy-write (left-node lazy-node opts) buffer opts)
            (.putInt buffer (.getLevel lazy-node opts))
            (.putInt buffer (.getCnt lazy-node opts))
            (.writeValue f lazy-node buffer opts)
            (lazy-write (right-node lazy-node opts) buffer opts)))
        (.limit new-bb (lazy-byte-length lazy-node opts))
        (compare-and-set! (get-buffer-atom lazy-node) nil new-bb)
        (reset! (get-data-atom lazy-node) nil)))))

(defn lazy-read [^ByteBuffer buffer opts]
  (let [^ByteBuffer bb (.slice buffer)
        id (.get bb)]
    (if (= id (byte \n))
      (do (.get buffer)
          (create-lazy-empty-node))
      (let [f (factory-for-id id opts)
            bb (.slice buffer)
            _ (.get buffer)
            lm5 (.getInt buffer)
            _ (.position buffer (+ lm5 (.position buffer)))
            blen (+ 5 lm5)
            _ (.limit bb blen)]
        (->LazyNode
         (atom nil)
         (atom nil)
         (atom blen)
         (atom bb)
         f)))))

(defn- get-lazy-data [^LazyNode this opts]
  (if (empty-node? this)
    emptyNode
    (let [a (get-data-atom this)]
      (when (nil? @a)
        (let [bb (.slice (get-buffer this))
              _ (.position bb 5)
              left (lazy-read bb opts)
              level (long (.getInt bb))
              cnt (long (.getInt bb))
              t2 (.deserialize (get-factory this) this bb opts)
              right (lazy-read bb opts)]
          (compare-and-set! a nil (Node. t2 level left right cnt))))
      @a)))

(def ^LazyNode emptyLazyNode
  (->LazyNode
   (atom emptyNode)
   (atom nil)
   (atom 1)
   (atom nil)
   (factory-for-id
     (byte \n)
     {:factory-registry default-factory-registry})))

(defn create-lazy-empty-node
  [] emptyLazyNode)

(defn load-lazy-vector [buffer opts]
  (if (:factory-registry opts)
    (let [r (vector-opts opts)]
      (new AAVector (node-read buffer r) r))
    (let [r (assoc opts :factory-registry default-factory-registry)
          r (vector-opts r)]
      (new AAVector (node-read buffer r) r))))

(defn load-lazy-sorted-map [buffer opts]
  (let [r opts
        r (if (:comparator r)
            r
            (assoc r :comparator RT/DEFAULT_COMPARATOR))
        r (if (:factory-registry r)
            r
            (assoc r :factory-registry default-factory-registry))
        r (map-opts r)]
    (new AAMap (node-read buffer r) r)))

(defn load-lazy-sorted-set [buffer opts]
  (let [r opts
        r (if (:comparator r)
            r
            (assoc r :comparator RT/DEFAULT_COMPARATOR))
        r (if (:factory-registry r)
            r
            (assoc r :factory-registry default-factory-registry))
        r (set-opts r)]
    (new AASet
         (new AAMap (node-read buffer r) r))))
