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
         virtual-write
         virtual-as-reference)

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

(defn ^VirtualNode value-node [^VirtualNode virtual-node opts]
  (if (empty-node? virtual-node)
    virtual-node)
  (let [^IFactory f (.factory virtual-node)
        vn (.valueNode f virtual-node opts)]
    (if vn
      vn
      (.getNada virtual-node))))

(defn- unchanged? [^VirtualNode virtual-node]
  @(.-buffer_atom virtual-node))

(defn- search-unchanged [unchanged ^VirtualNode virtual-node opts]
  (if (empty-node? virtual-node)
    unchanged
    (if (unchanged? virtual-node)
      (conj unchanged virtual-node)
      (-> unchanged
          (search-unchanged (value-node virtual-node opts) opts)
          (search-unchanged (left-node virtual-node opts) opts)
          (search-unchanged (right-node virtual-node opts) opts)))))

(defn- dropped-blocks [unused ^VirtualNode virtual-node unchanged opts]
  (if (empty-node? virtual-node)
    unused
    (if (contains? unchanged virtual-node)
      unused
      (let [^ByteBuffer bb @(.-buffer_atom virtual-node)
            unused (if (and bb (= 1 (.get bb 5)))
                        (conj unused (.getLong bb 6))
                        unused)
            unused (dropped-blocks unused (value-node virtual-node opts) unchanged opts)
            unused (dropped-blocks unused (left-node virtual-node opts) unchanged opts)
            unused (dropped-blocks unused (right-node virtual-node opts) unchanged opts)]
        unused))))

(defn find-dropped-blocks [old-node new-node opts]
  (dropped-blocks [] old-node (search-unchanged #{} new-node opts) opts))

(defn- new-byte-length [^VirtualNode virtual-node opts]
  (+ 1                                   ;node id
     4                                   ;byte length - 5
     1                                   ;reference flag
     (virtual-byte-length (left-node virtual-node opts) opts) ;left node
     4                                   ;level
     4                                   ;cnt
     (.valueLength (get-factory virtual-node) virtual-node opts) ;t2
     (virtual-byte-length (right-node virtual-node opts) opts)))

(defn shrinker [^VirtualNode virtual-node opts]
  (let [blen (new-byte-length virtual-node opts)]
    (if (>= (:db-block-size opts) blen)
      blen
      (let [largest-node (left-node virtual-node opts)
            nlen (virtual-byte-length largest-node opts)
            nx-node (right-node virtual-node opts)
            largest-node (if (< nlen (virtual-byte-length nx-node opts))
                           nx-node
                           largest-node)
            nlen (virtual-byte-length largest-node opts)
            nx-node (value-node virtual-node opts)
            largest-node (if (< nlen (virtual-byte-length nx-node opts))
                           nx-node
                           largest-node)]
        (virtual-as-reference largest-node opts)
        (recur virtual-node opts)))))

(defn virtual-byte-length [^VirtualNode virtual-node opts]
  (if (empty-node? virtual-node)
    1
    (let [a (.blenAtom virtual-node)
          blen @a]
      (if (nil? blen)
        (let [^ByteBuffer bb @(.bufferAtom virtual-node)
              blen (if bb
                     (.limit bb)
                     (shrinker virtual-node opts))] ;right node
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
  (println "virtual-as-reference!")
  (let [db-block-size (:db-block-size opts)
        bl (virtual-byte-length virtual-node opts)
        _ (println "bl" bl)
        _ (if (< db-block-size bl)
            (throw (Exception. (str "byte-length exceeds block size: " bl))))
        ^ByteBuffer nbb (ByteBuffer/allocate bl)
        _ (virtual-write virtual-node nbb opts)
        _ (.flip nbb)
        block-position ((:db-allocate opts) opts)
        ^FileChannel db-file-channel (:db-file-channel opts)
        _ (.write db-file-channel nbb (long block-position))
        _ (.flip nbb)
        blen (+ 1                                           ;bode id
                4                                           ;byte-length - 5
                1                                           ;reference flag
                8                                           ;block position
                4                                           ;block length
                32)                                         ;checksum
        ^ByteBuffer bb (ByteBuffer/allocate blen)
        ^IFactory f (.factory virtual-node)]
    (.put bb (byte (.factoryId f)))
    (.putInt bb (- blen 5))
    (.put bb (byte 1))
    (.putLong bb block-position)
    (.putInt bb bl)
    (put-cs256 bb (compute-cs256 nbb))
    (.flip bb)
    (reset! (get-buffer-atom virtual-node) bb)
    (reset! (.blenAtom virtual-node) blen)))

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

(defn fetch [^ByteBuffer bb opts]
  (let [block-position (.getLong bb)
        block-length (.getInt bb)
        ocs (get-cs256 bb)
        ^ByteBuffer nbb (ByteBuffer/allocate block-length)
        ^FileChannel db-file-channel (:db-file-channel opts)
        _ (.read db-file-channel nbb (long block-position))
        _ (.flip nbb)
        cs (compute-cs256 nbb)
        _ (if (not= ocs cs)
            (throw (Exception. "corrupted database")))
        ]
    (.flip nbb)
    (.position nbb 6)
    nbb))

(defn- get-virtual-data [^VirtualNode this opts]
  (if (empty-node? this)
    emptyNode
    (let [a (get-data-atom this)]
      (when (nil? @a)
        (let [bb (.slice (get-buffer this))
              _ (.position bb 5)
              reference-flag (.get bb)
              ^ByteBuffer bb (if (= reference-flag 0)
                   bb
                   (fetch bb opts))
              left (virtual-read bb opts)
              level (long (.getInt bb))
              cnt (long (.getInt bb))
              t2 (.deserialize (get-factory this) this bb opts)
              right (virtual-read bb opts)]
          (compare-and-set! a nil (->Node t2 level left right cnt))))
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
