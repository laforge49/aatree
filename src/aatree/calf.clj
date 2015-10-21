(ns aatree.calf
  (:require [aatree.core :refer :all])
  (:import (java.nio.channels FileChannel)
           (java.io File)
           (java.nio.file OpenOption StandardOpenOption)
           (java.nio ByteBuffer)
           (clojure.lang Agent)))

(set! *warn-on-reflection* true)

(defn- calf-writer [db-state aamap opts]
  (let [transaction-count (:transaction-count db-state)
        block-size (:block-size opts)
        position (* block-size (mod transaction-count 2))
        db-state (assoc db-state :transaction-count (+ 1 transaction-count))
        db-state (assoc db-state :data aamap)
        ^ByteBuffer bb (ByteBuffer/allocate block-size)
        ^FileChannel file-channel (:file-channel opts)
        data-size (byte-length aamap)]
    (if (< block-size (+ 4 4 8 data-size 32))
      (throw (Exception. "block-size exceeded on write")))
    (.putInt bb block-size)
    (.putInt bb data-size)
    (.putLong bb transaction-count)
    (put-aa bb aamap)
    (put-cs256 bb (compute-cs256 (.flip (.duplicate bb))))
    (.position file-channel (long position))
    (.flip bb)
    (.write file-channel bb)
    db-state))

(defn calf-send-write [aamap opts]
  (let [^Agent db-agent (:db-agent opts)]
    (send-off db-agent calf-writer aamap opts)))

(defn calf-write [aamap opts]
  (calf-send-write aamap opts)
  (let [send-write-timeout (:send-write-timeout opts)
        db-agent (:db-agent opts)]
    (if send-write-timeout
      (await-for send-write-timeout db-agent)
      (await db-agent))))

(defn- calf-new [opts]
  (let [data (new-sorted-map opts)
        db-state {:transaction-count 0 :data data}
        db-agent-options (get opts :db-agent-options [])
        db-agent (apply agent db-state db-agent-options)
        opts (assoc opts :db-agent db-agent)]
    (calf-write data opts)
    (calf-write data opts)
  opts))

(defn- calf-read [position opts]
  (let [^FileChannel file-channel (:file-channel opts)
        _ (.position file-channel (long position))
        block-size (:block-size opts)
        ^ByteBuffer bb (ByteBuffer/allocate block-size)
        _ (.limit bb 16)
        _ (.read file-channel bb)
        _ (.flip bb)
        _ (if (not= block-size (.getInt bb))
            (throw (Exception. "db block-size is" block-size)))
        data-size (.getInt bb)
        _ (if (< block-size (+ 4 4 8 data-size 32))
            (throw (Exception. "block-size exceeded on read")))
        transaction-count (.getLong bb)
        input-size (+ (.limit bb) data-size 32)
        _ (.limit bb input-size)
        _ (.read file-channel bb)
        _ (.flip bb)
        csp (- input-size 32)
        _ (.limit bb csp)
        cs (compute-cs256 bb)
        _ (.limit bb input-size)
        _ (println (.position bb) (.limit bb))
        ocs (get-cs256 bb)
        ]))

(defn- calf-old [opts]
  (let [block-size (:block-size opts)
        state0 (calf-read 0 opts)
        state1 (calf-read block-size opts)]
    opts))

(defn calf-open
  ([file block-size] (calf-open file block-size {}))
  ([^File file block-size opts]
   (if (:file-channel opts)
     opts
     (let [opts (assoc opts :file file)
           opts (assoc opts :block-size block-size)
           file-channel
           (FileChannel/open (.toPath file)
                             (into-array OpenOption
                                         [StandardOpenOption/CREATE
                                          StandardOpenOption/READ
                                          StandardOpenOption/WRITE
                                          StandardOpenOption/SYNC]))
           opts (assoc opts :file-channel file-channel)
           opts (if (has-aafactories opts)
                  opts
                  (lazy-opts opts))
           opts (assoc opts :root-header-size (+ 4 4 8 32))
;           _ (println (.size file-channel))
           opts (if (= 0 (.size file-channel))
                  (calf-new opts)
                  (calf-old opts))]
       opts))))

(defn calf-transaction-count [opts]
  (:transaction-count @(:db-agent opts)))

(defn calf-get [opts]
  (:data @(:db-agent opts)))