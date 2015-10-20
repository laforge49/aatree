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
        ^FileChannel file-channel (:file-channel opts)]
    (.putInt bb block-size)
    (.putInt bb (byte-length aamap))
    (.putLong bb transaction-count)
    (put-bytebuffer aamap bb)
    (put-cs256 bb (cs256 (.flip (.duplicate bb))))
    (.position file-channel (long position))
    (.write file-channel bb)
    db-state))

(defn calf-send-write [aamap opts]
  (let [^Agent db-agent (:db-agent opts)]
    (send db-agent calf-writer aamap opts)))

(defn calf-write [aamap opts]
  (calf-send-write aamap opts)
  (await (:db-agent opts)))

(defn- calf-new [opts]
  (let [data (new-sorted-map opts)
        db-state {:transaction-count 0 :data data}
        db-agent (agent db-state)
        opts (assoc opts :db-agent db-agent)]
    (calf-send-write data opts)
    (calf-send-write data opts)
  opts))

(defn- calf-old [opts]
  opts)

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
           opts (if (= 0 (.size file-channel))
                  (calf-new opts)
                  (calf-old opts))]
       opts))))

(defn transaction-count [opts]
  (:transaction-count @(:db-agent opts)))