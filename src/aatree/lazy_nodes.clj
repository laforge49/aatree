(ns aatree.lazy-nodes
  (:require [aatree.nodes :refer :all])
  (:import (java.nio ByteBuffer CharBuffer)))

(set! *warn-on-reflection* true)

(declare ->LazyNode
         ^aatree.nodes.INode get-data
         factory-for-instance
         create-lazy-empty-node)

(deftype LazyNode [data-atom sval-atom buffer-atom factory]

  aatree.nodes.INode

  (newNode [this t2 level left right cnt resources]
    (let [d (->Node t2 level left right cnt)]
      (->LazyNode (atom d) (atom nil) (atom nil) (factory-for-instance (:factory-registry resources) t2))))

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
  (sval [^aatree.lazy_nodes.LazyNode lazyNode resources])
  (byteLength [lazyNode resources])
  (deserialize [lazyNode resources])
  (write [lazyNode
         ^java.nio.ByteBuffer buffer
          resources])
  (read [lazyNode
         ^java.nio.ByteBuffer buffer
         resources]))

(defn- ^aatree.lazy_nodes.IFactory get-factory [^LazyNode lazy-node]
  (.-factory lazy-node))

(defn- get-buffer-atom [^LazyNode lazy-node]
  (.-buffer_atom lazy-node))

(defn- ^java.nio.ByteBuffer get-buffer [^LazyNode lazy-node]
  @(.-buffer-atom lazy-node))

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
        (.limit new-bb (.byteLength f lazy-node resources))
        (compare-and-set! (get-buffer-atom lazy-node) nil new-bb)))))

(defn node-byte-length [lazy-node resources] (.byteLength (get-factory lazy-node) lazy-node resources))

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

(defn read-lazy-node [buffer, resources])

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

(defn- deserialize [^LazyNode this resources]
  (let [d (.deserialize (get-factory this) this resources)
        a (.-data-atom this)]
    (compare-and-set! a nil d)
    @a))

(defn- get-data [^LazyNode this resources]
  (let [d @(.-data-atom this)]
    (if (nil? d)
      (deserialize this resources)
      d)))

(register-factory
  default-factory-registry
  (reify aatree.lazy_nodes.IFactory
    (factoryId [this] (byte \e))
    (instanceType [this] nil)
    (qualified [this t2] this)
    (sval [this lazyNode resources]
      (let [sval-atom (.-sval-atom lazyNode)]
        (if (nil? @sval-atom)
          (compare-and-set! sval-atom nil (pr-str (.getT2 lazyNode resources))))
        @sval-atom))
    (byteLength [this lazyNode resources]
      (+ 1 ;node id
         4 ;byte length - 5
         (node-byte-length (left-node lazyNode resources) resources) ;left node
         4 ;sval length
         (* 2 (count (.sval this lazyNode resources))) ;sval
         (node-byte-length (left-node lazyNode resources) resources))) ;right node
    (deserialize [this lazyNode resources])
    (write [this lazyNode buffer resources]
      (.put buffer (byte (.factoryId this)))
      (.putInt buffer (- (.byteLength this lazyNode resources) 5))
      (node-write (left-node lazyNode resources) buffer resources)
      (let [^String sv (.sval this lazyNode resources)
            svl (count sv)
            ^CharBuffer cb (.asCharBuffer buffer)]
        (.putInt buffer svl)
        (.put cb sv)
        (.position buffer (+ (* 2 svl) (.position buffer))))
      (node-write (right-node lazyNode resources) buffer resources))
    (read [this lazyNode buffer resources])))

(def ^LazyNode emptyLazyNode
  (->LazyNode
    (atom (create-empty-node))
    (atom "")
    (atom nil)
    (reify aatree.lazy_nodes.IFactory
      (factoryId [this] (byte \n))
      (instanceType [this] nil)
      (qualified [this t2] this)
      (sval [this lazyNode resources]
        "")
      (byteLength [this lazyNode resources]
        1)
      (deserialize [this lazyNode resources])
      (write [this lazyNode buffer resources]
        (.put buffer (byte (.factoryId this))))
      (read [this lazyNode buffer resources]))))

(register-factory
  default-factory-registry
  (.factory emptyLazyNode))

(defn create-lazy-empty-node
  [] emptyLazyNode)
