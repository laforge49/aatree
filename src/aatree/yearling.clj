(ns aatree.yearling
  (:require [aatree.core :refer :all])
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

(defn- max-blocks [opts] (quot (:max-db-size opts) (:db-block-size opts)))

(defn- max-allocated-longs [opts] (quot (+ (max-blocks opts) 7) 8))

(defn- yearling-updater [db-state app-updater opts]
  (let [uber-map (:uber-map db-state)]
    (binding [*allocated* (:allocated db-state)
              *transaction-count* (+ (:transaction-count db-state) 1)
              *release-pending* (:release-pending uber-map)
              *time-millis* (System/currentTimeMillis)]
      (try
        (let [app-map (app-updater (:app-map uber-map) opts)
              uber-map (assoc uber-map :app-map app-map)
              uber-map (assoc uber-map :release-pending *release-pending*)
              db-block-size (:db-block-size opts)
              max-db-size (:max-db-size opts)
              position (* db-block-size (mod (:transaction-count db-state) 2))
              ^ByteBuffer bb (ByteBuffer/allocate db-block-size)
              ^FileChannel file-channel (:db-file-channel opts)
              allocated-long-array (.toLongArray *allocated*)
              ala-len (alength allocated-long-array)
              mx-allocated-longs (max-allocated-longs opts)
              _ (if (< mx-allocated-longs ala-len)
                  (throw (Exception. (str "allocated size exceeded on write: " mx-allocated-longs ", " ala-len))))
              map-size (byte-length uber-map)
              uber-map (if (< db-block-size (+ 4 8 4 4 8 map-size (* mx-allocated-longs 8) 32))
                  ((:as-reference opts) uber-map opts)
                  uber-map)
              map-size (byte-length uber-map)
              _ (if (< db-block-size (+ 4 8 4 4 8 map-size (* mx-allocated-longs 8) 32))
                  (throw (Exception. (str "block-size exceeded on write: " map-size))))
              db-state (assoc db-state :transaction-count *transaction-count*)
              db-state (assoc db-state :uber-map uber-map)
              db-state (assoc db-state :allocated *allocated*)
              ]
          (.putInt bb db-block-size)
          (.putLong bb max-db-size)
          (.putInt bb map-size)
          (.putInt bb ala-len)
          (.putLong bb *transaction-count*)
          (put-aa bb uber-map)
          (.put (.asLongBuffer bb) allocated-long-array)
          (.position bb (+ (.position bb) (* ala-len 8)))
          (put-cs256 bb (compute-cs256 (.flip (.duplicate bb))))

          ;(println map-size (+ 4 8 4 4 8 map-size (* mx-allocated-longs 8) 32))

          (.flip bb)
          (.write file-channel bb (long position))
          db-state)
        (catch Exception e
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
      (await-for send-write-timeout db-agent)
      (await db-agent))))

(defn- create-db-agent [db-state opts]
  (assoc opts :db-agent (apply agent db-state (get opts :db-agent-options []))))

(defn yearling-null-updater [aamap opts]
  aamap)

(defn- yearling-new [opts]
  (let [uber-map (new-sorted-map opts)
        uber-map (assoc uber-map :release-pending (new-vector opts))
        uber-map (assoc uber-map :app-map (new-sorted-map opts))
        ^BitSet allocated (BitSet.)
        _ (.set allocated 0)
        _ (.set allocated 1)
        db-state {:transaction-count 0 :uber-map uber-map :allocated allocated}
        opts (create-db-agent db-state opts)]
    (yearling-update yearling-null-updater opts)
    (yearling-update yearling-null-updater opts)
    opts))

(defn- yearling-read [position opts]
  (let [^FileChannel file-channel (:db-file-channel opts)
        db-block-size (:db-block-size opts)
        max-db-size (:max-db-size opts)
        ^ByteBuffer bb (ByteBuffer/allocate db-block-size)
        _ (.limit bb (+ 4 8 4 4 8))
        _ (.read file-channel bb (long position))
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
              _ (if (< db-block-size (+ 4 8 4 4 8 map-size (* mx-allocated-longs 8) 32))
                  (throw (Exception. "block-size exceeded on read")))
              transaction-count (.getLong bb)
              input-size (+ (.limit bb) map-size (* ala-len 8) 32)
              _ (.limit bb input-size)
              _ (.read file-channel bb (long (+ position 4 8 4 4 8)))
              _ (.flip bb)
              csp (- input-size 32)
              _ (.limit bb csp)
              cs (compute-cs256 bb)
              _ (.limit bb input-size)
              ocs (get-cs256 bb)
              _ (.position bb (+ 4 8 4 4 8))
              uber-map (load-sorted-map bb opts)
              la (long-array ala-len)
              _ (.get (.asLongBuffer bb) (longs la))
              allocated (BitSet/valueOf (longs la))]
          (if (not= cs ocs)
            nil
            {:transaction-count transaction-count
             :uber-map          uber-map
             :allocated         allocated}))))))

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
      (throw (Exception. "block-position is not at start of block")))
    (if (not (.get *allocated* block))
      (throw (Exception. "block has not been allocated")))
    (set! *release-pending* (conj *release-pending* vec))))

(defn- yearling-process-pending [age trans opts]
  (if (not (empty? *release-pending*))
    (let [oldest (*release-pending* 0)]
      (when (and (<= (+ (oldest 0) age) *time-millis*)
                 (<= (+ (oldest 1) trans) *transaction-count*))
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
  ([file db-block-size max-db-size] (yearling-open file db-block-size max-db-size {}))
  ([^File file db-block-size max-db-size opts]
   (if (:db-file-channel opts)
     opts
     (let [opts (assoc opts :db-close yearling-close)
           opts (assoc opts :db-get-sorted-map yearling-get-sorted-map)
           opts (assoc opts :db-transaction-count yearling-transaction-count)
           opts (assoc opts :db-send yearling-send)
           opts (assoc opts :db-update yearling-update)
           opts (assoc opts :db-file file)
           opts (assoc opts :db-block-size db-block-size)
           opts (assoc opts :max-db-size max-db-size)
           opts (assoc opts :db-allocated yearling-allocated)
           opts (assoc opts :db-allocate yearling-allocate)
           opts (assoc opts :db-release-pending yearling-release-pending)
           opts (assoc opts :db-release yearling-release)
           opts (assoc opts :db-process-pending yearling-process-pending)
           file-channel
           (FileChannel/open (.toPath file)
                             (into-array OpenOption
                                         [StandardOpenOption/CREATE
                                          StandardOpenOption/READ
                                          StandardOpenOption/WRITE
                                          StandardOpenOption/SYNC]))
           opts (assoc opts :db-file-channel file-channel)
           opts (if (has-aafactories opts)
                  opts
                  (virtual-opts opts))
           opts (if (= 0 (.size file-channel))
                  (yearling-new opts)
                  (yearling-old opts))]
       opts))))
