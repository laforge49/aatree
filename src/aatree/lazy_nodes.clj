(ns aatree.lazy-nodes
  (:require [aatree.nodes :refer :all]))

(set! *warn-on-reflection* true)

(declare ->LazyNode
         ^aatree.nodes.INode get-data
         factory-for-instance)

(deftype LazyNode [data-atom sval-atom buffer-atom factory-registry factory]

  aatree.nodes.INode

  (newNode [this t2 level left right cnt]
    (let [d (->Node t2 level left right cnt (empty-node this))]
      (->LazyNode (atom d) (atom nil) (atom nil) factory-registry (factory-for-instance factory-registry t2))))

  (getT2 [this] (.getT2 (get-data this)))

  (getLevel [this] (.getLevel (get-data this)))

  (getLeft [this] (.getLeft (get-data this)))

  (getRight [this] (.getRight (get-data this)))

  (getCnt [this] (.getCnt (get-data this)))

  (getNada [this] (.getNada (get-data this))))

(definterface IFactory
  (qualified [t2])
  (sval [^aatree.lazy_nodes.LazyNode lazyNode])
  (byteLength [lazyNode])
  (deserialize [lazyNode])
  (write [lazyNode buffer])
  (read [lazyNode buffer]))

(defn- ^IFactory get-factory [^LazyNode lazy-node]
  (.-factory lazy-node))

(deftype factory-registry [by-id-atom by-type-atom])

(defn ^factory-registry create-factory-registry []
  (factory-registry. (atom {}) (atom {})))

(def default-factory-registry (create-factory-registry))

(defn- ^IFactory factory-for-id [^factory-registry fregistry id]
  (let [f (@(.-by_id_atom fregistry) id)]
  (if (nil? f)
    (factory-for-id fregistry "e")
    f)))

(defn className [^Class c] (.getName c))

(defn ^IFactory factory-for-type [^factory-registry fregistry typ]
  (let [f (@(.-by_type_atom fregistry) typ)]
    (if (nil? f)
      (factory-for-id fregistry "e")
      f)))

(defn- ^IFactory factory-for-instance [^factory-registry fregistry inst]
  (let [typ (type inst)
        f (factory-for-type fregistry typ)
        q (.qualified f inst)]
    (if (nil? q)
      (throw (UnsupportedOperationException. (str "Unknown qualified durable type: " (className typ))))
      q)))

(defn register-factory [^factory-registry fregistry ^IFactory factory id typ]
  (swap! (.-by-id-atom fregistry) assoc id factory)
  (if typ
    (swap! (.-by-type-atom fregistry) assoc typ factory)))

(defn- deserialize [^LazyNode this]
  (let [d (.deserialize (get-factory this) this)
        a (.-data-atom this)]
    (compare-and-set! a nil d)
    @a))

(defn- get-data [^LazyNode this]
  (let [d @(.-data-atom this)]
    (if (nil? d)
      (deserialize this)
      d)))

(register-factory
  default-factory-registry
  (reify IFactory
    (qualified [this t2] this)
    (sval [this lazyNode]
      (let [sval-atom (.-sval-atom lazyNode)]
      (if (nil? @sval-atom)
        (compare-and-set! sval-atom nil (pr-str (.getT2 lazyNode))))
      @sval-atom))
    (byteLength [this lazyNode])
    (deserialize [this lazyNode])
    (write [this lazyNode buffer])
    (read [this lazyNode buffer]))
  "e"
  nil)

(def lazy-node (->LazyNode (atom (create-empty-node)) (atom nil) (atom nil) default-factory-registry nil))

(defn create-lazy-empty-node
  ([] lazy-node)
  ([factory-registry] (->LazyNode (atom (create-empty-node)) (atom nil) (atom nil) factory-registry nil)))
