(ns aatree.lazy-nodes
  (:require [aatree.nodes :refer :all])
  (:import (java.nio ByteBuffer CharBuffer)
           (aatree.nodes Node INode IFactory AAContext WrapperNode)
           (clojure.lang MapEntry PersistentVector RT)
           (aatree AAVector AAMap AASet)))

(set! *warn-on-reflection* true)

(declare ->LazyNode
         ^aatree.nodes.INode get-lazy-data
         create-lazy-empty-node)

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

  (dataAtom [this] (.-data-atom this))

  (svalAtom [this] (.-sval-atom this))

  (blenAtom [this] (.-blen-atom this))

  (bufferAtom [this] (.-buffer-atom this))

  (factory [this] (.-factory this)))

(def default-lazy-factory-registry (create-factory-registry))

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

(def ^AAContext lazy-vector-context
  (let [class-atom (atom {})
        factory-atom (atom nil)]
    (reify AAContext
      (classAtom [this] class-atom)
      (getDefaultFactory [this] @factory-atom)
      (setDefaultFactory
        [this f]
        (compare-and-set! factory-atom nil f))
      (refineInstance [this inst] inst))))

(def ^AAContext lazy-map-context
  (let [class-atom (atom {})
        factory-atom (atom nil)]
    (reify AAContext
      (classAtom [this] class-atom)
      (getDefaultFactory [this] @factory-atom)
      (setDefaultFactory
        [this f]
        (compare-and-set! factory-atom nil f))
      (refineInstance [this inst]
        (let [^MapEntry map-entry inst]
          (.getValue map-entry))))))

(def ^AAContext lazy-set-context
  (let [class-atom (atom {})
        factory-atom (atom nil)]
    (reify AAContext
      (classAtom [this] class-atom)
      (getDefaultFactory [this] @factory-atom)
      (setDefaultFactory
        [this f]
        (compare-and-set! factory-atom nil f))
      (refineInstance [this inst]
        (let [^MapEntry map-entry inst]
          (.getKey map-entry))))))

(def ^LazyNode emptyLazyNode
  (->LazyNode
   (atom emptyNode)
   (atom nil)
   (atom 1)
   (atom nil)
   (reify IFactory
     (factoryId [this] (byte \n));;;;;;;;;;;;;;;;;;;;;;;; n - nil content
     (instanceClass [this] nil)
     (qualified [this t2 opts] this))))

(register-factory default-lazy-factory-registry nil (.factory emptyLazyNode))

(defn create-lazy-empty-node
  [] emptyLazyNode)

(defn lazy-vector-opts [opts]
  (assoc opts :aacontext lazy-vector-context))

(defn lazy-map-opts [opts]
  (assoc opts :aacontext lazy-map-context))

(defn lazy-set-opts [opts]
  (assoc opts :aacontext lazy-set-context))

(register-factory
 default-lazy-factory-registry
 lazy-vector-context
 (reify IFactory
   (factoryId [this] (byte \e));;;;;;;;;;;;;;;;;;;;;; e - vector default factory
   (instanceClass [this] nil)
   (qualified [this t2 opts] this)
   (sval [this inode opts]
     (default-sval this inode opts))
   (valueLength [this node opts]
     (default-valueLength this node opts))
   (deserialize [this node bb opts]
     (deserialize-sval this node bb opts))
   (writeValue [this node buffer opts]
     (default-write-value this node buffer opts))))

(.setDefaultFactory
 lazy-vector-context
 (factory-for-id
  (byte \e)
  {:factory-registry default-lazy-factory-registry}))

(register-factory
 default-lazy-factory-registry
 lazy-vector-context
 (reify IFactory
   (factoryId [this] (byte \v));;;;;;;;;;;;;;;;;;;;;;;;;;; v aavector in aavector
   (instanceClass [this] aatree.AAVector)
   (qualified [this t2 opts] this)
   (valueLength [this node opts]
     (let [^AAVector v (.getT2 node opts)]
       (lazy-byte-length (get-inode v) (get-opts v))))
   (deserialize [this node bb opts]
     (let [opts (lazy-vector-opts opts)]
       (new AAVector (lazy-read bb opts) opts)))
   (writeValue [this node buffer opts]
     (let [^AAVector v (.getT2 node opts)]
       (lazy-write (get-inode v) buffer (get-opts v))))))

(register-factory
 default-lazy-factory-registry
 lazy-vector-context
 (reify IFactory
   (factoryId [this] (byte \m));;;;;;;;;;;;;;;;;;;;;;;;;;; m aamap in aavector
   (instanceClass [this] aatree.AAMap)
   (qualified [this t2 opts] this)
   (valueLength [this node opts]
     (let [^AAMap m (.getT2 node opts)]
       (lazy-byte-length (get-inode m) (get-opts m))))
   (deserialize [this node bb opts]
     (let [opts (lazy-map-opts opts)]
       (new AAMap (lazy-read bb opts) opts)))
   (writeValue [this node buffer opts]
     (let [^AAMap v (.getT2 node opts)]
       (lazy-write (get-inode v) buffer (get-opts v))))))

(register-factory
 default-lazy-factory-registry
 lazy-vector-context
 (reify IFactory
   (factoryId [this] (byte \s));;;;;;;;;;;;;;;;;;;;;;;;;;; s aaset in aavector
   (instanceClass [this] aatree.AASet)
   (qualified [this t2 opts] this)
   (valueLength [this node opts]
     (let [^AAMap m (.getT2 node opts)]
       (lazy-byte-length (get-inode m) (get-opts m))))
   (deserialize [this node bb opts]
     (let [opts (lazy-map-opts opts)]
       (new AASet (new AAMap (lazy-read bb opts) opts))))
   (writeValue [this node buffer opts]
     (let [^AASet s (.getT2 node opts)]
       (lazy-write (get-inode s) buffer (get-opts s))))))

(register-factory
 default-lazy-factory-registry
 lazy-map-context
 (reify IFactory
   (factoryId [this] (byte \p));;;;;;;;;;;;;;;;;;;;;;;;;;; p - map default factory
   (instanceClass [this] nil)
   (qualified [this t2 opts] this)
   (sval [this inode opts]
     (default-sval this inode opts))
   (valueLength [this node opts]
     (default-valueLength this node opts))
   (deserialize [this node bb opts]
     (let [^PersistentVector v (deserialize-sval this node bb opts)
           t2 (MapEntry. (.get v 0) (.get v 1))]
       t2))
   (writeValue [this node buffer opts]
     (default-write-value this node buffer opts))))

(.setDefaultFactory
 lazy-map-context
 (factory-for-id
  (byte \p)
  {:factory-registry default-lazy-factory-registry}))

(register-factory
 default-lazy-factory-registry
 lazy-map-context
 (reify IFactory
   (factoryId [this] (byte \V));;;;;;;;;;;;;;;;;;;;;;;;;;; V aavector in aamap
   (instanceClass [this] aatree.AAVector)
   (qualified [this t2 opts] this)
   (sval [this inode opts]
     (key-sval this inode opts))
   (valueLength [this node opts]
     (let [^MapEntry map-entry (.getT2 node opts)
           ^AAVector v (.getValue map-entry)]
       (+ (default-valueLength this node opts)
          (lazy-byte-length (get-inode v) (get-opts v)))))
   (deserialize [this node bb opts]
     (let [k (deserialize-sval this node bb opts)
           opts (lazy-vector-opts opts)
           v (new AAVector (lazy-read bb opts) opts)]
       (MapEntry. k v)))
   (writeValue [this node buffer opts]
     (default-write-value this node buffer opts)
     (let [^MapEntry map-entry (.getT2 node opts)
           ^AAVector v (.getValue map-entry)]
       (lazy-write (get-inode v) buffer (get-opts v))))))

(register-factory
 default-lazy-factory-registry
 lazy-map-context
 (reify IFactory
   (factoryId [this] (byte \M));;;;;;;;;;;;;;;;;;;;;;;;;;; M aamap in aamap
   (instanceClass [this] aatree.AAMap)
   (qualified [this t2 opts] this)
   (sval [this inode opts]
     (key-sval this inode opts))
   (valueLength [this node opts]
     (let [^MapEntry map-entry (.getT2 node opts)
           ^AAMap m (.getValue map-entry)]
       (+ (default-valueLength this node opts)
          (lazy-byte-length (get-inode m) (get-opts m)))))
   (deserialize [this node bb opts]
     (let [k (deserialize-sval this node bb opts)
           opts (lazy-map-opts opts)
           v (new AAMap (lazy-read bb opts) opts)]
       (MapEntry. k v)))
   (writeValue [this node buffer opts]
     (default-write-value this node buffer opts)
     (let [^MapEntry map-entry (.getT2 node opts)
           ^AAMap m (.getValue map-entry)]
       (lazy-write (get-inode m) buffer (get-opts m))))))

(register-factory
 default-lazy-factory-registry
 lazy-map-context
 (reify IFactory
   (factoryId [this] (byte \S));;;;;;;;;;;;;;;;;;;;;;;;;;; S aaset in aamap
   (instanceClass [this] aatree.AASet)
   (qualified [this t2 opts] this)
   (sval [this inode opts]
     (key-sval this inode opts))
   (valueLength [this node opts]
     (let [^MapEntry map-entry (.getT2 node opts)
           ^AASet s (.getValue map-entry)]
       (+ (default-valueLength this node opts)
          (lazy-byte-length (get-inode s) (get-opts s)))))
   (deserialize [this node bb opts]
     (let [k (deserialize-sval this node bb opts)
           opts (lazy-set-opts opts)
           v (new AASet (new AAMap (lazy-read bb opts) opts))]
       (MapEntry. k v)))
   (writeValue [this node buffer opts]
     (default-write-value this node buffer opts)
     (let [^MapEntry map-entry (.getT2 node opts)
           ^AASet s (.getValue map-entry)]
       (lazy-write (get-inode s) buffer (get-opts s))))))

(register-factory
 default-lazy-factory-registry
 lazy-set-context
 (reify IFactory
   (factoryId [this] (byte \q));;;;;;;;;;;;;;;;;;;;;;;;;;; q - set default factory
   (instanceClass [this] nil)
   (qualified [this t2 opts] this)
   (sval [this inode opts]
     (key-sval this inode opts))
   (valueLength [this node opts]
     (default-valueLength this node opts))
   (deserialize [this node bb opts]
     (let [k (deserialize-sval this node bb opts)]
       (MapEntry. k k)))
   (writeValue [this node buffer opts]
     (default-write-value this node buffer opts))))

(.setDefaultFactory
 lazy-set-context
 (factory-for-id
  (byte \q)
  {:factory-registry default-lazy-factory-registry}))

(defn load-lazy-vector [buffer opts]
  (if (:factory-registry opts)
    (let [r (lazy-vector-opts opts)]
      (new AAVector (lazy-read buffer r) r))
    (let [r (assoc opts :factory-registry default-lazy-factory-registry)
          r (lazy-vector-opts r)]
      (new AAVector (lazy-read buffer r) r))))

(defn load-lazy-sorted-map [buffer opts]
  (let [r opts
        r (if (:comparator r)
            r
            (assoc r :comparator RT/DEFAULT_COMPARATOR))
        r (if (:factory-registry r)
            r
            (assoc r :factory-registry default-lazy-factory-registry))
        r (lazy-map-opts r)]
    (new AAMap (lazy-read buffer r) r)))

(defn load-lazy-sorted-set [buffer opts]
  (let [r opts
        r (if (:comparator r)
            r
            (assoc r :comparator RT/DEFAULT_COMPARATOR))
        r (if (:factory-registry r)
            r
            (assoc r :factory-registry default-lazy-factory-registry))
        r (lazy-set-opts r)]
    (new AASet
         (new AAMap (lazy-read buffer r) r))))
