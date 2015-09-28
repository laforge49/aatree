(ns aatree.lazy-nodes
  (:require [aatree.nodes :refer :all])
  (:import (java.nio ByteBuffer CharBuffer)
           (aatree.nodes Node)
           (clojure.lang MapEntry PersistentVector)))

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

  (newNode [this t2 level left right cnt resources]
    (let [d (->Node t2 level left right cnt)
          f (factory-for-instance (:factory-registry resources) t2)]
      (->LazyNode (atom d) (atom nil) (atom nil) (atom nil) f)))

  (getT2 [this resources] (.getT2 (get-data this resources) resources))

  (getLevel [this resources] (.getLevel (get-data this resources) resources))

  (getLeft [this resources] (.getLeft (get-data this resources) resources))

  (getRight [this resources] (.getRight (get-data this resources) resources))

  (getCnt [this resources] (.getCnt (get-data this resources) resources))

  (getNada [this] (create-lazy-empty-node)))

(definterface IFactory
  (factoryId [])
  (instanceType [])
  (qualified [t2])
  (sval [^aatree.nodes.INode inode resources])
  (valueLength [^aatree.lazy_nodes.LazyNode lazyNode resources])
  (deserialize [^aatree.lazy_nodes.LazyNode lazyNode
                ^java.nio.ByteBuffer buffer
                resources])
  (write [^aatree.lazy_nodes.LazyNode lazyNode
          ^java.nio.ByteBuffer buffer
          resources])
  (read [^java.nio.ByteBuffer buffer
         resources]))

(defn- ^aatree.lazy_nodes.IFactory get-factory [^LazyNode lazy-node]
  (.-factory lazy-node))

(defn- get-buffer-atom [^LazyNode lazy-node]
  (.-buffer_atom lazy-node))

(defn- ^java.nio.ByteBuffer get-buffer [^LazyNode lazy-node]
  @(.-buffer-atom lazy-node))

(defn- get-data-atom [^LazyNode this] (.-data-atom this))

(deftype factory-registry [by-id-atom by-type-atom])

(defn ^factory-registry create-factory-registry
  ([]
   (factory-registry. (atom {})
                      (atom {})))
  ([^factory-registry fregistry]
   (factory-registry. (atom @(.-by_id_atom fregistry))
                      (atom @(.-by_type_atom fregistry)))))

(def default-factory-registry (create-factory-registry))

(defn- ^aatree.lazy_nodes.IFactory factory-for-id [^factory-registry fregistry id]
  (let [f (@(.-by_id_atom fregistry) id)]
    (if (nil? f)
      (factory-for-id fregistry (byte \e))
      f)))

(defn className [^Class c] (.getName c))

(defn ^aatree.lazy_nodes.IFactory factory-for-type [^factory-registry fregistry typ]
  (let [f (@(.-by_type_atom fregistry) typ)]
    (if (nil? f)
      (factory-for-id fregistry (byte \e))
      f)))

(defn- ^aatree.lazy_nodes.IFactory factory-for-instance [^factory-registry fregistry inst]
  (let [typ (type inst)
        f (factory-for-type fregistry typ)
        q (.qualified f inst)]
    (if (nil? q)
      (throw (UnsupportedOperationException. (str "Unknown qualified durable type: " (className typ))))
      q)))

(defn register-factory [^factory-registry fregistry ^aatree.lazy_nodes.IFactory factory]
  (swap! (.-by-id-atom fregistry) assoc (.factoryId factory) factory)
  (let [typ (.instanceType factory)]
    (if typ
      (swap! (.-by-type-atom fregistry) assoc typ factory))))

(defn- str-val [^IFactory factory ^aatree.lazy_nodes.LazyNode lazyNode resources]
  (let [sval-atom (.-sval-atom lazyNode)]
    (if (nil? @sval-atom)
      (compare-and-set! sval-atom nil (.sval factory lazyNode resources)))
    @sval-atom))

(defn- default-sval [this ^aatree.nodes.INode inode resources]
  (pr-str (.getT2 inode resources)))

(defn node-byte-length [^LazyNode lazy-node resources]
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
                        (node-byte-length (left-node lazy-node resources) resources) ;left node
                        4 ;level
                        4 ;cnt
                        (.valueLength (get-factory lazy-node) lazy-node resources) ;t2
                        (node-byte-length (right-node lazy-node resources) resources)))] ;right node
          (compare-and-set! a nil blen)))
      @a)))

(defn- default-valueLength [this ^LazyNode lazyNode resources]
  (+ 4 ;sval length
     (* 2 (count (str-val this lazyNode resources))))) ;sval

(defn node-write [^LazyNode lazy-node ^ByteBuffer buffer resources]
  (let [^IFactory f (.-factory lazy-node)
        ^ByteBuffer old-bb (get-buffer lazy-node)]
    (if old-bb
      (let [new-bb (.duplicate old-bb)
            lim (.limit new-bb)
            ba (byte-array lim)]
        (.get new-bb ba)
        (.put buffer ba))
      (let [new-bb (.slice buffer)]
        (.write f lazy-node buffer resources)
        (.limit new-bb (node-byte-length lazy-node resources))
        (compare-and-set! (get-buffer-atom lazy-node) nil new-bb)
        (reset! (get-data-atom lazy-node) nil)))))

(defn- default-write [^IFactory this
                      ^aatree.lazy_nodes.LazyNode lazyNode
                      ^java.nio.ByteBuffer buffer
                      resources]
  (.put buffer (byte (.factoryId this)))
  (.putInt buffer (- (node-byte-length lazyNode resources) 5))
  (node-write (left-node lazyNode resources) buffer resources)
  (.putInt buffer (.getLevel lazyNode resources))
  (.putInt buffer (.getCnt lazyNode resources))
  (let [^String sv (str-val this lazyNode resources)
        svl (count sv)
        _ (.putInt buffer svl)
        ^CharBuffer cb (.asCharBuffer buffer)]
    (.put cb sv)
    (.position buffer (+ (* 2 svl) (.position buffer))))
  (node-write (right-node lazyNode resources) buffer resources))

(defn node-read [^ByteBuffer buffer resources]
  (let [^ByteBuffer bb (.slice buffer)
        id (.get bb)
        r (:factory-registry resources)
        f (factory-for-id r id)]
    (.read f buffer resources)))

(defn- default-read [this
                     ^java.nio.ByteBuffer buffer
                     resources]
  (let [bb (.slice buffer)
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
     this)))

(defn- get-data [^LazyNode this resources]
  (if (empty-node? this)
    emptyNode
    (let [a (get-data-atom this)]
      (when (nil? @a)
        (let [bb (.slice (get-buffer this))
              _ (.position bb 5)
              left (node-read bb resources)
              level (.getInt bb)
              cnt (.getInt bb)
              t2 (.deserialize (get-factory this) this bb resources)
              right (node-read bb resources)]
          (compare-and-set! a nil (Node. t2 level left right cnt))))
      @a)))

(register-factory
 default-factory-registry
 (reify aatree.lazy_nodes.IFactory
   (factoryId [this] (byte \e))
   (instanceType [this] (type LazyNode))
   (qualified [this t2] this)
   (sval [this inode resources]
     (default-sval this inode resources))
   (valueLength [this lazyNode resources]
     (default-valueLength this lazyNode resources))
   (deserialize [this lazyNode bb resources]
     (let [svl (.getInt bb)
           ^CharBuffer cb (.asCharBuffer bb)
           svc (char-array svl)
           _ (.get cb svc)
           sv (String. svc)
           _ (reset! (.-sval_atom lazyNode) sv)
           t2 (read-string resources sv)
           _ (.position bb (+ (.position bb) (* 2 svl)))]
       t2))
   (write [this lazyNode buffer resources]
     (default-write this lazyNode buffer resources))
   (read [this buffer resources]
     (default-read this buffer resources))))

(register-factory
 default-factory-registry
 (reify aatree.lazy_nodes.IFactory
   (factoryId [this] (byte \p))
   (instanceType [this] clojure.lang.MapEntry)
   (qualified [this t2] this)
   (sval [this inode resources]
     (default-sval this inode resources))
   (valueLength [this lazyNode resources]
     (default-valueLength this lazyNode resources))
   (deserialize [this lazyNode bb resources]
     (let [svl (.getInt bb)
           ^CharBuffer cb (.asCharBuffer bb)
           svc (char-array svl)
           _ (.get cb svc)
           sv (String. svc)
           _ (reset! (.-sval_atom lazyNode) sv)
           ^PersistentVector v (read-string resources sv)
           t2 (MapEntry. (.get v 0) (.get v 1))
           _ (.position bb (+ (.position bb) (* 2 svl)))]
       t2))
   (write [this lazyNode buffer resources]
     (default-write this lazyNode buffer resources))
   (read [this buffer resources]
     (default-read this buffer resources))))

(def ^LazyNode emptyLazyNode
  (->LazyNode
   (atom emptyNode)
   (atom nil)
   (atom 1)
   (atom nil)
   (reify aatree.lazy_nodes.IFactory
     (factoryId [this] (byte \n))
     (instanceType [this] nil)
     (qualified [this t2] this)
     (write [this lazyNode buffer resources]
       (.put buffer (byte (.factoryId this))))
     (read [this buffer resources]
       (.get buffer)
       (create-lazy-empty-node)))))

(register-factory
 default-factory-registry
 (.factory emptyLazyNode))

(defn create-lazy-empty-node
  [] emptyLazyNode)
