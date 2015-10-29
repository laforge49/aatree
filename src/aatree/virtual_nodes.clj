(ns aatree.virtual-nodes
  (:require [aatree.nodes :refer :all])
  (:import (java.nio ByteBuffer)
           (aatree.nodes Node IFactory WrapperNode)
           (clojure.lang RT)
           (aatree AAVector AAMap AASet)
           (java.nio.channels FileChannel)))

(set! *warn-on-reflection* true)

(declare ->VirtualNode
         ^aatree.nodes.INode get-virtual-data
         create-virtual-empty-node
         virtual-byte-length
         virtual-write)

(deftype VirtualNode [data-atom sval-atom blen-atom buffer-atom factory]

  aatree.nodes.INode

  (newNode [this t2 level left right cnt opts]
    (let [d (->Node t2 level left right cnt)
          f (factory-for-instance t2 opts)]
      (->VirtualNode (atom d) (atom nil) (atom nil) (atom nil) f)))

  (getT2 [this opts] (.getT2 (get-virtual-data this opts) opts))

  (^Long getLevel [this opts] (.getLevel (get-virtual-data this opts) opts))

  (getLeft [this opts] (.getLeft (get-virtual-data this opts) opts))

  (getRight [this opts] (.getRight (get-virtual-data this opts) opts))

  (^Long getCnt [this opts] (.getCnt (get-virtual-data this opts) opts))

  (getNada [this] (create-virtual-empty-node))

  WrapperNode

  (dataAtom [this] (.-data-atom this))

  (svalAtom [this] (.-sval-atom this))

  (blenAtom [this] (.-blen-atom this))

  (bufferAtom [this] (.-buffer-atom this))

  (factory [this] (.-factory this))

  (nodeByteLength [this opts] (virtual-byte-length this opts))

  (nodeWrite [this buffer opts] (virtual-write this buffer opts)))

(defn virtual-byte-length [^VirtualNode virtual-node opts]
  (if (empty-node? virtual-node)
    1
    (let [a (.blenAtom virtual-node)
          blen @a]
      (if (nil? blen)
        (let [^ByteBuffer bb @(.bufferAtom virtual-node)
              blen (if bb
                     (.limit bb)
                     (+ 1                                   ;node id
                        4                                   ;byte length - 5
                        1                                   ;reference flag
                        (virtual-byte-length (left-node virtual-node opts) opts) ;left node
                        4                                   ;level
                        4                                   ;cnt
                        (.valueLength (get-factory virtual-node) virtual-node opts) ;t2
                        (virtual-byte-length (right-node virtual-node opts) opts)))] ;right node
          (compare-and-set! a nil blen)))
      @a)))

(defn virtual-write [^VirtualNode virtual-node ^ByteBuffer buffer opts]
  (let [^IFactory f (.factory virtual-node)
        ^ByteBuffer old-bb (get-buffer virtual-node)]
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
            (.putInt buffer (- (virtual-byte-length virtual-node opts) 5))
            (.put buffer (byte 0))
            (virtual-write (left-node virtual-node opts) buffer opts)
            (.putInt buffer (.getLevel virtual-node opts))
            (.putInt buffer (.getCnt virtual-node opts))
            (.writeValue f virtual-node buffer opts)
            (virtual-write (right-node virtual-node opts) buffer opts)))
        (.limit new-bb (virtual-byte-length virtual-node opts))
        (compare-and-set! (get-buffer-atom virtual-node) nil new-bb)
        (reset! (get-data-atom virtual-node) nil)))))

(defn virtual-as-reference [^VirtualNode virtual-node opts]
  (let [db-block-size (:db-block-size opts)
        bl (virtual-byte-length virtual-node opts)
        _ (if (< db-block-size bl)
            (throw (Exception. (str "byte-length exceeds block size: " bl))))
        ^ByteBuffer bb (ByteBuffer/allocate db-block-size)
        _ (virtual-write virtual-node bb opts)
        _ (.flip bb)
        block-position ((:db-allocate opts) opts)
        ^FileChannel db-file-channel (:db-file-channel opts)
        _ (.write db-file-channel bb (long block-position))]
    (println "Ribbit!")
    virtual-node))

(defn virtual-read [^ByteBuffer buffer opts]
  (let [^ByteBuffer bb (.slice buffer)
        id (.get bb)]
    (if (= id (byte \n))
      (do (.get buffer)
          (create-virtual-empty-node))
      (let [f (factory-for-id id opts)
            bb (.slice buffer)
            _ (.get buffer)
            lm5 (.getInt buffer)
            _ (.position buffer (+ lm5 (.position buffer)))
            blen (+ 5 lm5)
            _ (.limit bb blen)]
        (->VirtualNode
          (atom nil)
          (atom nil)
          (atom blen)
          (atom bb)
          f)))))

(defn- get-virtual-data [^VirtualNode this opts]
  (if (empty-node? this)
    emptyNode
    (let [a (get-data-atom this)]
      (when (nil? @a)
        (let [bb (.slice (get-buffer this))
              _ (.position bb 6)
              left (virtual-read bb opts)
              level (long (.getInt bb))
              cnt (long (.getInt bb))
              t2 (.deserialize (get-factory this) this bb opts)
              right (virtual-read bb opts)]
          (compare-and-set! a nil (Node. t2 level left right cnt))))
      @a)))

(def ^VirtualNode emptyVirtualNode
  (->VirtualNode
    (atom emptyNode)
    (atom nil)
    (atom 1)
    (atom nil)
    (factory-for-id
      (byte \n)
      {:factory-registry default-factory-registry})))

(defn create-virtual-empty-node
  [] emptyVirtualNode)

(defn load-virtual-vector [buffer opts]
  (if (:factory-registry opts)
    (let [r (vector-opts opts)]
      (new AAVector (node-read buffer r) r))
    (let [r (assoc opts :factory-registry default-factory-registry)
          r (vector-opts r)]
      (new AAVector (node-read buffer r) r))))

(defn load-virtual-sorted-map [buffer opts]
  (let [r opts
        r (if (:comparator r)
            r
            (assoc r :comparator RT/DEFAULT_COMPARATOR))
        r (if (:factory-registry r)
            r
            (assoc r :factory-registry default-factory-registry))
        r (map-opts r)]
    (new AAMap (node-read buffer r) r)))

(defn load-virtual-sorted-set [buffer opts]
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
