(ns aatree.yearling
  (:require [aatree.core :refer :all]
            [aatree.nodes :refer :all]
            [aatree.db-file-trait :refer :all]
            [aatree.db-agent-trait :refer :all])
  (:import (java.nio ByteBuffer)
           (java.util BitSet)
           (java.io File)))

(set! *warn-on-reflection* true)

(def ^:dynamic ^BitSet *allocated*)
(def ^:dynamic *release-pending*)
(def ^:dynamic *time-millis*)
(def ^:dynamic *transaction-count*)
(def ^:dynamic *last-node-id*)

(declare yearling-release
         yearling-process-pending
         yearling-close)

(defn- max-blocks [this] (quot (:max-db-size this) (:db-block-size this)))

(defn- max-allocated-longs [this] (quot (+ (max-blocks this) 7) 8))

(defn- yearling-new-node-id []
  (set! *last-node-id* (+ 1 *last-node-id*)))

(defn- release-dropped-blocks [this old-uber-map uber-map]
  (let [uber-map (assoc uber-map :release-pending *release-pending*)
        dropped-blocks ((:find-dropped-blocks this)
                         (get-inode old-uber-map)
                         (get-inode uber-map)
                         this)]
    (if (empty? dropped-blocks)
      uber-map
      (do
        (reduce (fn [_ block-position] (yearling-release this block-position))
                nil
                dropped-blocks)
        (recur this uber-map uber-map)))))

(defn- yearling-updater [db-state this app-updater]
  (let [old-uber-map (:uber-map db-state)
        transaction-count (:transaction-count db-state)
        db-block-size (:db-block-size this)
        mx-allocated-longs (max-allocated-longs this)
        block-position (* db-block-size (mod transaction-count 2))
        max-db-size (:max-db-size this)]
    (binding [*allocated* (:allocated db-state)
              *transaction-count* (+ transaction-count 1)
              *last-node-id* (:last-node-id db-state)
              *release-pending* (:release-pending old-uber-map)
              *time-millis* (System/currentTimeMillis)]
      (try
        (yearling-process-pending this (:db-pending-age this) (:db-pending-count this))
        (let [app-map (:app-map old-uber-map)
              app-map (app-updater this app-map)
              uber-map (assoc old-uber-map :app-map app-map)

              uber-map (release-dropped-blocks this old-uber-map uber-map)
              map-size (byte-length uber-map)
              _ (when (< db-block-size (+ 4 8 4 4 8 8 map-size (* mx-allocated-longs 8) 32))
                  ((:as-reference this) (get-inode uber-map) this))
              map-size (byte-length uber-map)
              _ (if (< db-block-size (+ 4 8 4 4 8 8 map-size (* mx-allocated-longs 8) 32))
                  (throw (Exception. (str "block-size exceeded on write: " map-size))))

              map-size (byte-length uber-map)
              allocated-long-array (.toLongArray *allocated*)
              ala-len (alength allocated-long-array)
              _ (if (< mx-allocated-longs ala-len)
                  (throw (Exception. (str "allocated size exceeded on write: " mx-allocated-longs ", " ala-len))))
              ^ByteBuffer bb (ByteBuffer/allocate db-block-size)
              db-state (assoc db-state :transaction-count *transaction-count*)
              db-state (assoc db-state :last-node-id *last-node-id*)
              db-state (assoc db-state :uber-map uber-map)
              db-state (assoc db-state :allocated *allocated*)
              ]
          (.putInt bb db-block-size)
          (.putLong bb max-db-size)
          (.putInt bb map-size)
          (.putInt bb ala-len)
          (.putLong bb *transaction-count*)
          (.putLong bb *last-node-id*)
          (put-aa bb uber-map)
          (.put (.asLongBuffer bb) allocated-long-array)
          (.position bb (+ (.position bb) (* ala-len 8)))
          (put-cs256 bb (compute-cs256 (.flip (.duplicate bb))))
          (.flip bb)
          ((:db-file-write-root this) bb (long block-position))
          db-state)
        (catch Throwable e
          (.printStackTrace e)
          (throw e))))))

(defn yearling-null-updater [this aamap]
  aamap)

(defn- create-db-state [this]
  (binding [*last-node-id* 0]
    (let [uber-map (new-sorted-map this)
          uber-map (assoc uber-map :release-pending (new-vector this))
          uber-map (assoc uber-map :app-map (new-sorted-map this))
          ^BitSet allocated (BitSet.)
          _ (.set allocated 0)
          _ (.set allocated 1)
          db-state {:transaction-count 0
                    :last-node-id      *last-node-id*
                    :uber-map          uber-map
                    :allocated         allocated}
          ]
      db-state)))

(defn- yearling-new [this]
  (let [db-state (create-db-state this)
        db-state (yearling-updater db-state this yearling-null-updater)
        db-state (yearling-updater db-state this yearling-null-updater)]
    db-state))

(defn- yearling-read [this block-position]
  (let [db-block-size (:db-block-size this)
        max-db-size (:max-db-size this)
        ^ByteBuffer bb (ByteBuffer/allocate db-block-size)
        _ (.limit bb (+ 4 8 4 4 8 8))
        _ (db-file-read this bb (long block-position))
        _ (.flip bb)]
    (if (not= db-block-size (.getInt bb))
      nil
      (if (not= max-db-size (.getLong bb))
        nil
        (let [map-size (.getInt bb)
              ala-len (.getInt bb)
              mx-allocated-longs (max-allocated-longs this)
              _ (if (< mx-allocated-longs ala-len)
                  (throw (Exception. "allocated size exceeded on read")))
              _ (if (< db-block-size (+ 4 8 4 4 8 8 map-size (* mx-allocated-longs 8) 32))
                  (throw (Exception. "block-size exceeded on read")))
              transaction-count (.getLong bb)
              last-node-id (.getLong bb)
              input-size (+ (.limit bb) map-size (* ala-len 8) 32)
              _ (.limit bb input-size)
              _ (db-file-read this bb (long (+ block-position 4 8 4 4 8 8)))
              _ (.flip bb)
              csp (- input-size 32)
              _ (.limit bb csp)
              cs (compute-cs256 bb)
              _ (.limit bb input-size)
              ocs (get-cs256 bb)
              _ (.position bb (+ 4 8 4 4 8 8))
              uber-map (load-sorted-map bb this)
              la (long-array ala-len)
              _ (.get (.asLongBuffer bb) (longs la))
              allocated (BitSet/valueOf (longs la))]
          (if (not= cs ocs)
            nil
            {:transaction-count transaction-count
             :uber-map          uber-map
             :allocated         allocated
             :last-node-id      last-node-id}))))))

(defn- choose [state0 state1]
  (if state0
    (if state1
      (if (> (:transaction-count state0) (:transaction-count state1))
        state0
        state1)
      state0)
    (if state1
      state1
      (throw (Exception. "corrupted database")))))

(defn- yearling-old [this]
  (let [db-block-size (:db-block-size this)
        state0 (yearling-read this 0)
        state1 (yearling-read this db-block-size)]
    (choose state0 state1)))

(defn- yearling-transaction-count [this]
  (:transaction-count @(:db-agent this)))

(defn- yearling-get-sorted-map [this]
  (:app-map (:uber-map @(:db-agent this))))

(defn- yearling-allocated [this]
  (let [state @(:db-agent this)
        ^BitSet allocated (:allocated state)]
    (.cardinality allocated)))

(defn- yearling-allocate [this]
  (let [avail (.nextClearBit *allocated* 0)]
    (.set *allocated* avail)
    (* avail (:db-block-size this))))

(defn- yearling-release-pending [this]
  (:release-pending (:uber-map @(:db-agent this))))

(defn- yearling-release [this block-position]
  (let [db-block-size (:db-block-size this)
        block (quot block-position db-block-size)
        vec (new-vector this)
        vec (conj vec *time-millis* *transaction-count* block)]
    (if (not= 0 (mod block-position db-block-size))
      (throw (Exception. (str "block-position is not at start of block: " block-position))))
    (if (not (.get *allocated* block))
      (throw (Exception. (str "block has not been allocated: " block " " (:db-block-size this)))))
    (set! *release-pending* (conj *release-pending* vec))
    ))

(defn- yearling-process-pending [this age trans]
  (when (not (empty? *release-pending*))
    (let [oldest (*release-pending* 0)]
      (when (and (<= (+ (oldest 0) age) *time-millis*)
                 (<= (+ (oldest 1) trans) *transaction-count*))
        (if (not (.get *allocated* (oldest 2)))
          (throw (Exception. (str "already available: " (oldest 2)))))
        (.clear *allocated* (oldest 2))
        (set! *release-pending* (dropn *release-pending* 0))
        (recur this age trans)))))

(defn- create-initial-state [this]
  (choice this db-file-empty? yearling-new yearling-old))

(defn yearling-open
  ([file] (yearling-open {} file))
  ([this ^File file]
   (let [this (-> this
                  (db-file-open file)
                  (assoc :db-get-sorted-map yearling-get-sorted-map)
                  (assoc :db-transaction-count yearling-transaction-count)
                  (assoc :db-new-node-id yearling-new-node-id)
                  (assoc-default :db-block-size 500000)
                  (assoc-default :max-db-size 100000000000)
                  (default :create-db-chan db-agent)
                  (assoc :db-allocated yearling-allocated)
                  (assoc :db-allocate yearling-allocate)
                  (assoc :db-release-pending yearling-release-pending)
                  (assoc :db-release yearling-release)
                  (assoc :db-process-pending yearling-process-pending)
                  (assoc-default :db-pending-age 0)
                  (assoc-default :db-pending-count 2)
                  (default :new-sorted-map virtual-opts)
                  (assoc :db-updater yearling-updater))]
     (create-db-chan this create-initial-state))))
