(ns aatree.lazy-nodes
  (:require [aatree.nodes :refer :all])
  (:import (java.nio ByteBuffer CharBuffer)
           (aatree.nodes Node INode)
           (clojure.lang MapEntry PersistentVector)
           (aatree AAVector)))

(set! *warn-on-reflection* true)

(declare ->LazyNode
         ^aatree.nodes.INode get-data
         factory-for-instance
         create-lazy-empty-node
         node-byte-length
         node-write
         node-read)

(deftype LazyNode [data-atom sval-atom blen-atom buffer-atom factory]

  aatree.nodes.INode

  (newNode [this t2 level left right cnt opts]
    (let [d (->Node t2 level left right cnt)
          f (factory-for-instance t2 opts)]
      (->LazyNode (atom d) (atom nil) (atom nil) (atom nil) f)))

  (getT2 [this opts] (.getT2 (get-data this opts) opts))

  (^Long getLevel [this opts] (.getLevel (get-data this opts) opts))

  (getLeft [this opts] (.getLeft (get-data this opts) opts))

  (getRight [this opts] (.getRight (get-data this opts) opts))

  (^Long getCnt [this opts] (.getCnt (get-data this opts) opts))

  (getNada [this] (create-lazy-empty-node)))

(definterface IFactory
  (factoryId [])
  (instanceClass [])
  (qualified [t2 opts])
  (sval [^aatree.nodes.INode inode opts])
  (valueLength [^aatree.lazy_nodes.LazyNode lazyNode opts])
  (deserialize [^aatree.lazy_nodes.LazyNode lazyNode
                ^java.nio.ByteBuffer buffer
                opts])
  (writeValue [^aatree.lazy_nodes.LazyNode lazyNode
          ^java.nio.ByteBuffer buffer
          opts]))

(defn- ^aatree.lazy_nodes.IFactory get-factory [^LazyNode lazy-node]
  (.-factory lazy-node))

(defn- get-buffer-atom [^LazyNode lazy-node]
  (.-buffer_atom lazy-node))

(defn- ^java.nio.ByteBuffer get-buffer [^LazyNode lazy-node]
  @(.-buffer-atom lazy-node))

(defn- get-data-atom [^LazyNode this] (.-data-atom this))

(deftype factory-registry [by-id-atom by-class-atom])

(defn ^factory-registry create-factory-registry
  ([]
   (factory-registry. (atom {})
                      (atom {})))
  ([^factory-registry fregistry]
   (factory-registry. (atom @(.-by_id_atom fregistry))
                      (atom @(.by_class_atom fregistry)))))

(def default-factory-registry (create-factory-registry))

(definterface AAContext
  (classAtom [])
  (getDefaultFactory [])
  (setDefaultFactory [factory])
  (refineInstance [inst]))

(defn- ^IFactory factory-for-id [id opts]
  (let [^factory-registry r (:factory-registry opts)
        f (@(.-by_id_atom r) id)]
    (if (nil? f)
      (let [^AAContext context (:aacontext opts)]
        (.getDefaultFactory context))
      f)))

(defn- register-class [^AAContext aacontext ^IFactory factory]
  (let [clss (.instanceClass factory)]
    (if clss
      (swap! (.classAtom aacontext) assoc clss factory))))

(defn ^IFactory factory-for-class [^AAContext aacontext clss opts]
  (let [f (@(.classAtom aacontext) clss)]
    (if (nil? f)
      (let [^AAContext context (:aacontext opts)]
        (.getDefaultFactory context))
      f)))

(defn className [^Class c] (.getName c))

(defn- ^IFactory factory-for-instance [inst opts]
  (let [^AAContext aacontext (:aacontext opts)
        inst (.refineInstance aacontext inst)
        clss (class inst)
        f (factory-for-class aacontext clss opts)
        q (.qualified f inst opts)]
    (if (nil? q)
      (throw (UnsupportedOperationException. (str "Unknown qualified durable class: " (className clss))))
      q)))

(defn register-factory [^factory-registry fregistry
                        ^AAContext aacontext
                        ^IFactory factory]
  (swap! (.-by-id-atom fregistry) assoc (.factoryId factory) factory)
  (register-class aacontext factory))

(defn- str-val [^IFactory factory ^LazyNode lazyNode opts]
  (let [sval-atom (.-sval-atom lazyNode)]
    (if (nil? @sval-atom)
      (compare-and-set! sval-atom nil (.sval factory lazyNode opts)))
    @sval-atom))

(defn- default-sval [this ^INode inode opts]
  (pr-str (.getT2 inode opts)))

(defn node-byte-length [^LazyNode lazy-node opts]
  (if (empty-node? lazy-node)
    1
    (let [a (.-blen-atom lazy-node)
          blen @a]
      (if (nil? blen)
        (let [^ByteBuffer bb @(.-buffer-atom lazy-node)
              blen (if bb
                     (.limit bb)
                     (+ 1 ;node id
                        4 ;byte length - 5
                        (node-byte-length (left-node lazy-node opts) opts) ;left node
                        4 ;level
                        4 ;cnt
                        (.valueLength (get-factory lazy-node) lazy-node opts) ;t2
                        (node-byte-length (right-node lazy-node opts) opts)))] ;right node
          (compare-and-set! a nil blen)))
      @a)))

(defn- default-valueLength [this ^LazyNode lazyNode opts]
  (+ 4 ;sval length
     (* 2 (count (str-val this lazyNode opts))))) ;sval

(defn node-write [^LazyNode lazy-node ^ByteBuffer buffer opts]
  (let [^IFactory f (.-factory lazy-node)
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
            (.putInt buffer (- (node-byte-length lazy-node opts) 5))
            (node-write (left-node lazy-node opts) buffer opts)
            (.putInt buffer (.getLevel lazy-node opts))
            (.putInt buffer (.getCnt lazy-node opts))
            (.writeValue f lazy-node buffer opts)
            (node-write (right-node lazy-node opts) buffer opts)))
        (.limit new-bb (node-byte-length lazy-node opts))
        (compare-and-set! (get-buffer-atom lazy-node) nil new-bb)
        (reset! (get-data-atom lazy-node) nil)))))

(defn- default-write-value [^IFactory f
                      ^LazyNode lazy-node
                      ^ByteBuffer buffer
                      opts]
  (let [^String sv (str-val f lazy-node opts)
        svl (count sv)
        _ (.putInt buffer svl)
        ^CharBuffer cb (.asCharBuffer buffer)]
    (.put cb sv)
    (.position buffer (+ (* 2 svl) (.position buffer)))))

(defn node-read [^ByteBuffer buffer opts]
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

(defn- get-data [^LazyNode this opts]
  (if (empty-node? this)
    emptyNode
    (let [a (get-data-atom this)]
      (when (nil? @a)
        (let [bb (.slice (get-buffer this))
              _ (.position bb 5)
              left (node-read bb opts)
              level (long (.getInt bb))
              cnt (long (.getInt bb))
              t2 (.deserialize (get-factory this) this bb opts)
              right (node-read bb opts)]
          (compare-and-set! a nil (Node. t2 level left right cnt))))
      @a)))

(def ^AAContext vector-context
  (let [class-atom (atom {})
        factory-atom (atom nil)]
    (reify AAContext
      (classAtom [this] class-atom)
      (getDefaultFactory [this] @factory-atom)
      (setDefaultFactory
        [this f]
        (compare-and-set! factory-atom nil f))
      (refineInstance [this inst] inst))))

(def ^AAContext map-context
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

(defn vector-opts [opts]
  (assoc opts :aacontext vector-context))

(defn map-opts [opts]
  (assoc opts :aacontext map-context))

(register-factory
 default-factory-registry
 vector-context
 (reify aatree.lazy_nodes.IFactory
   (factoryId [this] (byte \e));;;;;;;;;;;;;;;;;;;;;; e - vector default factory
   (instanceClass [this] nil)
   (qualified [this t2 opts] this)
   (sval [this inode opts]
     (default-sval this inode opts))
   (valueLength [this lazyNode opts]
     (default-valueLength this lazyNode opts))
   (deserialize [this lazyNode bb opts]
     (let [svl (.getInt bb)
           ^CharBuffer cb (.asCharBuffer bb)
           svc (char-array svl)
           _ (.get cb svc)
           sv (String. svc)
           _ (reset! (.-sval_atom lazyNode) sv)
           t2 (read-string opts sv)
           _ (.position bb (+ (.position bb) (* 2 svl)))]
       t2))
   (writeValue [this lazyNode buffer opts]
     (default-write-value this lazyNode buffer opts))))

(.setDefaultFactory
  vector-context
  (factory-for-id
    (byte \e)
    {:factory-registry default-factory-registry}))

(register-factory
  default-factory-registry
  map-context
  (reify aatree.lazy_nodes.IFactory
    (factoryId [this] (byte \p));;;;;;;;;;;;;;;;;;;;;;;;;;; p - map default factory
    (instanceClass [this] nil)
    (qualified [this t2 opts] this)
    (sval [this inode opts]
      (default-sval this inode opts))
    (valueLength [this lazyNode opts]
      (default-valueLength this lazyNode opts))
    (deserialize [this lazyNode bb opts]
      (let [svl (.getInt bb)
            ^CharBuffer cb (.asCharBuffer bb)
            svc (char-array svl)
            _ (.get cb svc)
            sv (String. svc)
            _ (reset! (.-sval_atom lazyNode) sv)
            ^PersistentVector v (read-string opts sv)
            t2 (MapEntry. (.get v 0) (.get v 1))
            _ (.position bb (+ (.position bb) (* 2 svl)))]
        t2))
    (writeValue [this lazyNode buffer opts]
      (default-write-value this lazyNode buffer opts))))

(.setDefaultFactory
  map-context
  (factory-for-id
    (byte \p)
    {:factory-registry default-factory-registry}))

(register-factory
  default-factory-registry
  vector-context
  (reify aatree.lazy_nodes.IFactory
    (factoryId [this] (byte \v));;;;;;;;;;;;;;;;;;;;;;;;;;; v aavector in vector
    (instanceClass [this] aatree.AAVector)
    (qualified [this t2 opts] this)
    (valueLength [this lazyNode opts]
      (let [^aatree.AAVector v (.getT2 lazyNode opts)]
        (node-byte-length (get-inode v) (get-opts v))))
    (deserialize [this lazyNode bb opts]
      (let [opts (vector-opts opts)]
        (new AAVector (node-read bb opts) opts)))
    (writeValue [this lazyNode buffer opts]
      (let [^aatree.AAVector v (.getT2 lazyNode opts)]
        (node-write (get-inode v) buffer (get-opts v))))))

(def ^LazyNode emptyLazyNode
  (->LazyNode
   (atom emptyNode)
   (atom nil)
   (atom 1)
   (atom nil)
   (reify aatree.lazy_nodes.IFactory
     (factoryId [this] (byte \n));;;;;;;;;;;;;;;;;;;;;;;; n - nil content
     (instanceClass [this] nil)
     (qualified [this t2 opts] this))))

(register-factory default-factory-registry nil (.factory emptyLazyNode))

(defn create-lazy-empty-node
  [] emptyLazyNode)
