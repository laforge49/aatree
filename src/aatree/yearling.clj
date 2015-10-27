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

(defn- max-blocks [opts] (quot (:max-db-size opts) (:db-block-size opts)))

(defn- max-allocated-longs [opts] (quot (+ (max-blocks opts) 7) 8))

(defn- yearling-updater [db-state app-updater opts]
  (let [uber-map (:uber-map db-state)]
    (binding [*allocated* (:allocated db-state)
              *release-pending* (:release-pending uber-map)]
      (try
        (let [app-map (app-updater (:app-map uber-map) opts)
              uber-map (assoc uber-map :app-map app-map)
              uber-map (assoc uber-map :release-pending *release-pending*)
              transaction-count (:transaction-count db-state)
              block-size (:db-block-size opts)
              max-db-size (:max-db-size opts)
              position (* block-size (mod transaction-count 2))
              transaction-count (+ transaction-count 1)
              db-state (assoc db-state :transaction-count transaction-count)
              db-state (assoc db-state :uber-map uber-map)
              db-state (assoc db-state :allocated *allocated*)
              ^ByteBuffer bb (ByteBuffer/allocate block-size)
              ^FileChannel file-channel (:db-file-channel opts)
              map-size (byte-length uber-map)
              allocated-long-array (.toLongArray *allocated*)
              ala-len (alength allocated-long-array)
              mx-allocated-longs (max-allocated-longs opts)]
          (if (< mx-allocated-longs ala-len)
            (throw (Exception. "allocated size exceeded on write")))
          (if (< block-size (+ 4 8 4 4 8 map-size (* mx-allocated-longs 8) 32))
            (throw (Exception. "block-size exceeded on write")))
          (.putInt bb block-size)
          (.putLong bb max-db-size)
          (.putInt bb map-size)
          (.putInt bb ala-len)
          (.putLong bb transaction-count)
          (put-aa bb uber-map)
          (.put (.asLongBuffer bb) allocated-long-array)
          (.position bb (+ (.position bb) (* ala-len 8)))
          (put-cs256 bb (compute-cs256 (.flip (.duplicate bb))))
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
        block-size (:db-block-size opts)
        max-db-size (:max-db-size opts)
        ^ByteBuffer bb (ByteBuffer/allocate block-size)
        _ (.limit bb (+ 4 8 4 4 8))
        _ (.read file-channel bb (long position))
        _ (.flip bb)]
    (if (not= block-size (.getInt bb))
      nil
      (if (not= max-db-size (.getLong bb))
        nil
        (let [map-size (.getInt bb)
              ala-len (.getInt bb)
              mx-allocated-longs (max-allocated-longs opts)
              _ (if (< mx-allocated-longs ala-len)
                  (throw (Exception. "allocated size exceeded on read")))
              _ (if (< block-size (+ 4 8 4 4 8 map-size (* mx-allocated-longs 8) 32))
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
  (let [block-size (:db-block-size opts)
        state0 (yearling-read 0 opts)
        state1 (yearling-read block-size opts)]
    (create-db-agent (choose state0 state1) opts)))

(defn- yearling-transaction-count [opts]
  (:transaction-count @(:db-agent opts)))

(defn- yearling-get-sorted-map [opts]
  (:app-map (:uber-map @(:db-agent opts))))

(defn- yearling-allocated [opts]
  (let [state @(:db-agent opts)
        ^BitSet allocated (:allocated state)]
    (.cardinality allocated)))

(defn- yearling-release-pending [opts]
  (:release-pending (:uber-map @(:db-agent opts))))

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
           opts (assoc opts :db-release-pending yearling-release-pending)
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
                  (lazy-opts opts))
           opts (if (= 0 (.size file-channel))
                  (yearling-new opts)
                  (yearling-old opts))]
       opts))))
