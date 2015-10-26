(ns aatree.calf
  (:require [aatree.core :refer :all])
  (:import (java.nio.channels FileChannel)
           (java.io File)
           (java.nio.file OpenOption StandardOpenOption)
           (java.nio ByteBuffer)
           (clojure.lang Agent)))

(set! *warn-on-reflection* true)

(defn- calf-updater [db-state app-updater opts]
  (try
    (let [aamap (app-updater (:aamap db-state) opts)
          transaction-count (:transaction-count db-state)
          block-size (:db-block-size opts)
          position (* block-size (mod transaction-count 2))
          transaction-count (+ transaction-count 1)
          db-state (assoc db-state :transaction-count transaction-count)
          db-state (assoc db-state :aamap aamap)
          ^ByteBuffer bb (ByteBuffer/allocate block-size)
          ^FileChannel file-channel (:db-file-channel opts)
          map-size (byte-length aamap)]
      (if (< block-size (+ 4 4 8 map-size 32))
        (throw (Exception. "block-size exceeded on write")))
      (.putInt bb block-size)
      (.putInt bb map-size)
      (.putLong bb transaction-count)
      (put-aa bb aamap)
      (put-cs256 bb (compute-cs256 (.flip (.duplicate bb))))
      (.flip bb)
      (.write file-channel bb (long position))
      db-state)
    (catch Exception e
      (.printStackTrace e)
      (throw e))))

(defn- calf-send [app-updater opts]
  (let [^Agent db-agent (:db-agent opts)]
    (send-off db-agent calf-updater app-updater opts)))

(defn calf-update [app-updater opts]
  (db-send app-updater opts)
  (let [send-write-timeout (:send-update-timeout opts)
        db-agent (:db-agent opts)]
    (if send-write-timeout
      (await-for send-write-timeout db-agent)
      (await db-agent))))

(defn- create-db-agent [db-state opts]
  (assoc opts :db-agent (apply agent db-state (get opts :db-agent-options []))))

(defn calf-null-updater [aamap opts]
  aamap)

(defn- calf-new [opts]
  (let [db-state {:transaction-count 0 :aamap (new-sorted-map opts)}
        opts (create-db-agent db-state opts)]
    (calf-update calf-null-updater opts)
    (calf-update calf-null-updater opts)
    opts))

(defn- calf-read [position opts]
  (let [^FileChannel file-channel (:db-file-channel opts)
        block-size (:db-block-size opts)
        ^ByteBuffer bb (ByteBuffer/allocate block-size)
        _ (.limit bb (+ 4 4 8))
        _ (.read file-channel bb (long position))
        _ (.flip bb)]
    (if (not= block-size (.getInt bb))
      nil
      (let [map-size (.getInt bb)
            _ (if (< block-size (+ 4 4 8 map-size 32))
                (throw (Exception. "block-size exceeded on read")))
            transaction-count (.getLong bb)
            input-size (+ (.limit bb) map-size 32)
            _ (.limit bb input-size)
            _ (.read file-channel bb (long (+ position 16)))
            _ (.flip bb)
            csp (- input-size 32)
            _ (.limit bb csp)
            cs (compute-cs256 bb)
            _ (.limit bb input-size)
            ocs (get-cs256 bb)
            _ (.position bb (+ 4 4 8))
            _ (.limit bb csp)
            aamap (load-sorted-map bb opts)]
        (if (not= cs ocs)
          nil
          {:transaction-count transaction-count :aamap aamap})))))

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

(defn- calf-old [opts]
  (let [block-size (:db-block-size opts)
        state0 (calf-read 0 opts)
        state1 (calf-read block-size opts)]
    (create-db-agent (choose state0 state1) opts)))

(defn- calf-transaction-count [opts]
  (:transaction-count @(:db-agent opts)))

(defn- calf-get-sorted-map [opts]
  (:aamap @(:db-agent opts)))

(defn- calf-close [opts]
  (let [^FileChannel fc (:db-file-channel opts)]
    (if fc
      (do
        (.close fc)
        (assoc opts :db-file-channel nil))
      opts)))

(defn calf-open
  ([file block-size] (calf-open file block-size {}))
  ([^File file block-size opts]
   (if (:db-file-channel opts)
     opts
     (let [opts (assoc opts :db-close calf-close)
           opts (assoc opts :db-get-sorted-map calf-get-sorted-map)
           opts (assoc opts :db-transaction-count calf-transaction-count)
           opts (assoc opts :db-send calf-send)
           opts (assoc opts :db-update calf-update)
           opts (assoc opts :db-file file)
           opts (assoc opts :db-block-size block-size)
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
                  (calf-new opts)
                  (calf-old opts))]
       opts))))
