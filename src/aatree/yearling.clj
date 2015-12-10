(ns aatree.yearling
  (:require [aatree.core :refer :all]
            [aatree.nodes :refer :all]
            [aatree.db-file-trait :refer :all]
            [aatree.null-db-cache-trait :refer :all]
            [aatree.db-chan-trait :refer :all])
  (:import (java.nio ByteBuffer)
           (java.util BitSet)
           (java.io File)))

(set! *warn-on-reflection* true)

(declare yearling-release
         yearling-process-pending)

(defn- max-blocks [this] (quot (:max-db-size this) (:db-block-size this)))

(defn- max-allocated-longs [this] (quot (+ (max-blocks this) 7) 8))

(defn- yearling-new-node-id [this]
  (swap!
    (:last-node-id-atom this)
    (fn [old] (+ old 1))))

(defn- release-dropped-blocks [this old-uber-map uber-map]
  (let [dropped-blocks ((:find-dropped-blocks this)
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

(defn- yearling-updater [this app-updater]
  (let [old-uber-map (update-get this)
        db-block-size (:db-block-size this)
        mx-allocated-longs (max-allocated-longs this)
        block-position (* db-block-size (mod (get-transaction-count this) 2))
        _ (swap!
            (:transaction-count-atom this)
            (fn [old] (+ old 1)))
        max-db-size (:max-db-size this)]
    (vreset! (:time-millis-volatile this) (System/currentTimeMillis))
    (yearling-process-pending this (:db-pending-age this) (:db-pending-count this))
    (app-updater this)
    (let [uber-map (update-get this)

          uber-map (release-dropped-blocks this old-uber-map uber-map)
          map-size (byte-length uber-map)
          _ (when (< db-block-size (+ 4 8 4 4 8 8 map-size (* mx-allocated-longs 8) 32))
              ((:as-reference this) (get-inode uber-map) this))
          map-size (byte-length uber-map)
          _ (if (< db-block-size (+ 4 8 4 4 8 8 map-size (* mx-allocated-longs 8) 32))
              (throw (Exception. (str "block-size exceeded on write: " map-size))))

          map-size (byte-length uber-map)
          allocated-long-array (.toLongArray (get-allocated-bit-set this))
          ala-len (alength allocated-long-array)
          _ (if (< mx-allocated-longs ala-len)
              (throw
                (Exception.
                  (str "allocated size exceeded on write: " mx-allocated-longs ", " ala-len))))
          ^ByteBuffer bb (ByteBuffer/allocate db-block-size)]
      (vreset! (:db-update-vstate this) uber-map)
      (.putInt bb db-block-size)
      (.putLong bb max-db-size)
      (.putInt bb map-size)
      (.putInt bb ala-len)
      (.putLong bb (get-transaction-count this))
      (.putLong bb (get-last-node-id this))
      (put-aa bb uber-map)
      (.put (.asLongBuffer bb) allocated-long-array)
      (.position bb (+ (.position bb) (* ala-len 8)))
      (put-cs256 bb (compute-cs256 (.flip (.duplicate bb))))
      (.flip bb)
      ((:db-file-write-root this) bb (long block-position)))))

(defn yearling-null-updater [this])

(defn- yearling-new [this]
  (let [this (assoc this :transaction-count-atom (atom 0))
        ^BitSet allocated (BitSet.)
        _ (.set allocated 0)
        _ (.set allocated 1)
        this (assoc this :allocated-bit-set allocated)
        uber-map (new-sorted-map this)
        uber-map (assoc uber-map :release-pending (new-vector this))
        db-update-vstate (:db-update-vstate this)
        _ (vreset! db-update-vstate uber-map)
        _ (yearling-updater this yearling-null-updater)
        _ (yearling-updater this yearling-null-updater)
        uber-map @db-update-vstate]
    (vreset! db-update-vstate nil)
    [this uber-map]))

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

(defn- choose [this state0 state1]
  (let [state (if state0
                (if state1
                  (if (> (:transaction-count state0) (:transaction-count state1))
                    state0
                    state1)
                  state0)
                (if state1
                  state1
                  (throw (Exception. "corrupted database"))))
        this (assoc this :transaction-count-atom (atom (:transaction-count state)))
        this (assoc this :allocated-bit-set (:allocated state))]
    (reset! (:last-node-id-atom this) (:last-node-id state))
    [this (:uber-map state)]))

(defn- yearling-old [this]
  (let [db-block-size (:db-block-size this)
        state0 (yearling-read this 0)
        state1 (yearling-read this db-block-size)]
    (choose this state0 state1)))

(defn- yearling-allocated [this]
  (let [^BitSet allocated (get-allocated-bit-set this)]
    (.cardinality allocated)))

(defn- yearling-allocate [this]
  (let [^BitSet allocated (get-allocated-bit-set this)
        avail (.nextClearBit allocated 0)]
    (.set allocated avail)
    (* avail (:db-block-size this))))

(defn- yearling-release [this block-position]
  (let [db-block-size (:db-block-size this)
        block-nbr (quot block-position db-block-size)
        vec (new-vector this)
        vec (conj vec (get-time-millis this) (get-transaction-count this) block-nbr)]
    (if (not= 0 (mod block-position db-block-size))
      (throw (Exception. (str "block-position is not at start of block: " block-position))))
    (if (not (.get (get-allocated-bit-set this) block-nbr))
      (throw (Exception. (str "block has not been allocated: " block-nbr " " (:db-block-size this)))))
    (update-assoc-in! this [:release-pending] (conj (update-get-in this [:release-pending]) vec))
    (block-clear this block-nbr)))

(defn- yearling-process-pending [this age trans]
  (when-let [release-pending (update-get-in this [:release-pending])]
    (and release-pending (not (empty? release-pending))
         (let [allocated (get-allocated-bit-set this)
               oldest (release-pending 0)]
           (when (and (<= (+ (oldest 0) age) (get-time-millis this))
                      (<= (+ (oldest 1) trans) (get-transaction-count this)))
             (if (not (.get allocated (oldest 2)))
               (throw (Exception. (str "already available: " (oldest 2)))))
             (.clear allocated (oldest 2))
             (update-assoc-in! this [:release-pending] (dropn release-pending 0))
             (recur this age trans))))))

(defn yearling-open
  ([file] (yearling-open {} file))
  ([this ^File file]
   (let [this (-> this
                  (db-file-open file)
                  (assoc :db-new-node-id yearling-new-node-id)
                  (assoc-default :db-block-size 500000)
                  (assoc-default :max-db-size 100000000000)
                  (default :create-db-chan db-chan)
                  (default :block-clear null-db-cache)
                  (assoc :db-allocated yearling-allocated)
                  (assoc :db-allocate yearling-allocate)
                  (assoc :db-release yearling-release)
                  (assoc :db-process-pending yearling-process-pending)
                  (assoc-default :db-pending-age 0)
                  (assoc-default :db-pending-count 2)
                  (default :new-sorted-map virtual-opts)
                  (assoc :db-updater yearling-updater)
                  (assoc :last-node-id-atom (atom 0))
                  (assoc :time-millis-volatile (volatile! 0)))
         [this db-state] (choice this db-file-empty? yearling-new yearling-old)]
     (create-db-chan this db-state))))
