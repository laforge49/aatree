(ns aatree.yearling
  (:require [aatree.core :refer :all]
            [aatree.nodes :refer :all])
  (:import (java.nio ByteBuffer)
           (java.nio.channels FileChannel)
           (java.util BitSet)
           (clojure.lang Agent)
           (java.io File)
           (java.nio.file OpenOption StandardOpenOption)))


(set! *warn-on-reflection* true)

(def ^:dynamic ^BitSet *allocated*)
(def ^:dynamic *release-pending*)
(def ^:dynamic *time-millis*)
(def ^:dynamic *transaction-count*)
(def ^:dynamic *last-node-id*)

(declare yearling-release
         yearling-process-pending
         yearling-close)

(defn- max-blocks [opts] (quot (:max-db-size opts) (:db-block-size opts)))

(defn- max-allocated-longs [opts] (quot (+ (max-blocks opts) 7) 8))

(defn- release-dropped-blocks [old-uber-map uber-map opts]
  (let [uber-map (assoc uber-map :release-pending *release-pending*)
        dropped-blocks ((:find-dropped-blocks opts)
                         (get-inode old-uber-map)
                         (get-inode uber-map)
                         opts)]
    (if (empty? dropped-blocks)
      uber-map
      (do
        (reduce (fn [_ block-position] (yearling-release block-position opts))
                nil
                dropped-blocks)
        (recur uber-map uber-map opts)))))

(defn- yearling-new-node-id []
  (set! *last-node-id* (+ 1 *last-node-id*)))

(defn- yearling-updater [db-state app-updater opts]
  (let [old-uber-map (:uber-map db-state)]
    (binding [*allocated* (:allocated db-state)
              *transaction-count* (+ (:transaction-count db-state) 1)
              *last-node-id* (:last-node-id db-state)
              *release-pending* (:release-pending old-uber-map)
              *time-millis* (System/currentTimeMillis)]
      (try
        (let [app-map (:app-map old-uber-map)
              _ (yearling-process-pending (:db-pending-age opts) (:db-pending-count opts) opts)
              app-map (app-updater app-map opts)
              uber-map (assoc old-uber-map :app-map app-map)
              uber-map (release-dropped-blocks old-uber-map uber-map opts)
              db-block-size (:db-block-size opts)
              max-db-size (:max-db-size opts)
              block-position (* db-block-size (mod (:transaction-count db-state) 2))
              ^ByteBuffer bb (ByteBuffer/allocate db-block-size)
              ^FileChannel db-file-channel (:db-file-channel opts)
              allocated-long-array (.toLongArray *allocated*)
              ala-len (alength allocated-long-array)
              mx-allocated-longs (max-allocated-longs opts)
              _ (if (< mx-allocated-longs ala-len)
                  (throw (Exception. (str "allocated size exceeded on write: " mx-allocated-longs ", " ala-len))))
              map-size (byte-length uber-map)
              _ (if (< db-block-size (+ 4 8 4 4 8 8 map-size (* mx-allocated-longs 8) 32))
                  ((:as-reference opts) (get-inode uber-map) opts))
              map-size (byte-length uber-map)
              _ (if (< db-block-size (+ 4 8 4 4 8 8 map-size (* mx-allocated-longs 8) 32))
                  (throw (Exception. (str "block-size exceeded on write: " map-size))))
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
          (.write db-file-channel bb (long block-position))
          db-state)
        (catch Throwable e
          (yearling-close opts)
          (.printStackTrace e)
          (throw e))))))

(defn- yearling-send [app-updater opts]
  (let [^Agent db-agent (:db-agent opts)]
    (send-off db-agent yearling-updater app-updater opts)))

(defn yearling-update [app-updater opts]
  (db-send app-updater opts)
  (let [send-write-timeout (:send-update-timeout opts)
        db-agent (:db-agent opts)]
    (if send-write-timeout
      (if (not (await-for send-write-timeout db-agent))
        (throw (Exception. "timeout")))
      (await db-agent))))

(defn- create-db-agent [db-state opts]
  (assoc opts :db-agent (apply agent db-state (get opts :db-agent-options []))))

(defn yearling-null-updater [aamap opts]
  aamap)

(defn- create-db-state [opts]
  (binding [*last-node-id* 0]
    (let [uber-map (new-sorted-map opts)
          uber-map (assoc uber-map :release-pending (new-vector opts))
          uber-map (assoc uber-map :app-map (new-sorted-map opts))
          ^BitSet allocated (BitSet.)
          _ (.set allocated 0)
          _ (.set allocated 1)
          db-state {:transaction-count 0
                    :last-node-id      *last-node-id*
                    :uber-map          uber-map
                    :allocated         allocated}
          ]
      db-state)))


(defn- yearling-new [opts]
  (let [db-state (create-db-state opts)
        opts (create-db-agent db-state opts)]
    (yearling-update yearling-null-updater opts)
    (yearling-update yearling-null-updater opts)
    opts))

(defn- yearling-read [block-position opts]
  (let [^FileChannel db-file-channel (:db-file-channel opts)
        db-block-size (:db-block-size opts)
        max-db-size (:max-db-size opts)
        ^ByteBuffer bb (ByteBuffer/allocate db-block-size)
        _ (.limit bb (+ 4 8 4 4 8 8))
        _ (.read db-file-channel bb (long block-position))
        _ (.flip bb)]
    (if (not= db-block-size (.getInt bb))
      nil
      (if (not= max-db-size (.getLong bb))
        nil
        (let [map-size (.getInt bb)
              ala-len (.getInt bb)
              mx-allocated-longs (max-allocated-longs opts)
              _ (if (< mx-allocated-longs ala-len)
                  (throw (Exception. "allocated size exceeded on read")))
              _ (if (< db-block-size (+ 4 8 4 4 8 8 map-size (* mx-allocated-longs 8) 32))
                  (throw (Exception. "block-size exceeded on read")))
              transaction-count (.getLong bb)
              last-node-id (.getLong bb)
              input-size (+ (.limit bb) map-size (* ala-len 8) 32)
              _ (.limit bb input-size)
              _ (.read db-file-channel bb (long (+ block-position 4 8 4 4 8 8)))
              _ (.flip bb)
              csp (- input-size 32)
              _ (.limit bb csp)
              cs (compute-cs256 bb)
              _ (.limit bb input-size)
              ocs (get-cs256 bb)
              _ (.position bb (+ 4 8 4 4 8 8))
              uber-map (load-sorted-map bb opts)
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

(defn- yearling-old [opts]
  (let [db-block-size (:db-block-size opts)
        state0 (yearling-read 0 opts)
        state1 (yearling-read db-block-size opts)]
    (create-db-agent (choose state0 state1) opts)))

(defn- yearling-transaction-count [opts]
  (:transaction-count @(:db-agent opts)))

(defn- yearling-get-sorted-map [opts]
  (:app-map (:uber-map @(:db-agent opts))))

(defn- yearling-allocated [opts]
  (let [state @(:db-agent opts)
        ^BitSet allocated (:allocated state)]
    (.cardinality allocated)))

(defn- yearling-allocate [opts]
  (let [avail (.nextClearBit *allocated* 0)]
    (.set *allocated* avail)
    (* avail (:db-block-size opts))))

(defn- yearling-release-pending [opts]
  (:release-pending (:uber-map @(:db-agent opts))))

(defn- yearling-release [block-position opts]
  (let [db-block-size (:db-block-size opts)
        block (quot block-position db-block-size)
        vec (new-vector opts)
        vec (conj vec *time-millis* *transaction-count* block)]
    (if (not= 0 (mod block-position db-block-size))
      (throw (Exception. (str "block-position is not at start of block: " block-position))))
    (if (not (.get *allocated* block))
      (throw (Exception. (str "block has not been allocated: " block " " (:db-block-size opts)))))
    (set! *release-pending* (conj *release-pending* vec))
    ))

(defn- yearling-process-pending [age trans opts]
  (when (not (empty? *release-pending*))
    (let [oldest (*release-pending* 0)]
      (when (and (<= (+ (oldest 0) age) *time-millis*)
                 (<= (+ (oldest 1) trans) *transaction-count*))
        (if (not (.get *allocated* (oldest 2)))
          (throw (Exception. (str "already available: " (oldest 2)))))
        (.clear *allocated* (oldest 2))
        (set! *release-pending* (dropn *release-pending* 0))
        (recur age trans opts)))))

(defn- yearling-close [opts]
  (let [^FileChannel fc (:db-file-channel opts)]
    (if fc
      (do
        (.close fc)
        (assoc opts :db-file-channel nil))
      opts)))

(defn yearling-open
  ([file] (yearling-open file {}))
  ([^File file opts]
   (if (:db-file-channel opts)
     opts
     (let [opts (assoc opts :db-close yearling-close)
           opts (assoc opts :db-get-sorted-map yearling-get-sorted-map)
           opts (assoc opts :db-transaction-count yearling-transaction-count)
           opts (assoc opts :db-new-node-id yearling-new-node-id)
           opts (assoc opts :db-send yearling-send)
           opts (assoc opts :db-update yearling-update)
           opts (assoc opts :db-file file)
           opts (if (:db-block-size opts)
                  opts
                  (assoc opts :db-block-size 500000))
           opts (if (:max-db-size opts)
                  opts
                  (assoc opts :max-db-size 100000000000))
           opts (assoc opts :db-allocated yearling-allocated)
           opts (assoc opts :db-allocate yearling-allocate)
           opts (assoc opts :db-release-pending yearling-release-pending)
           opts (assoc opts :db-release yearling-release)
           opts (assoc opts :db-process-pending yearling-process-pending)
           opts (if (:db-pending-age opts)
                  opts
                  (assoc opts :db-pending-age 0))
           opts (if (:db-pending-count opts)
                  opts
                  (assoc opts :db-pending-count 2))
           db-file-channel
           (FileChannel/open (.toPath file)
                             (into-array OpenOption
                                         [StandardOpenOption/CREATE
                                          StandardOpenOption/READ
                                          StandardOpenOption/WRITE
                                          StandardOpenOption/SYNC]))
           opts (assoc opts :db-file-channel db-file-channel)
           opts (if (has-aafactories opts)
                  opts
                  (virtual-opts opts))
           opts (if (= 0 (.size db-file-channel))
                  (yearling-new opts)
                  (yearling-old opts))]
       opts))))
