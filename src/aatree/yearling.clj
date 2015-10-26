(ns aatree.yearling
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)
           (java.nio.channels FileChannel)
           (java.util BitSet)))


(set! *warn-on-reflection* true)

(def ^:dynamic ^BitSet *allocated*)

(def ^:dynamic *release-pending*)


(defn- yearling-updater [db-state app-updater opts]
  (binding [*allocated* (:allocated db-state)
            *release-pending* (:release-pending db-state)]
    (try
      (let [aamap (app-updater (:aamap db-state) opts)
            transaction-count (:transaction-count db-state)
            block-size (:db-block-size opts)
            position (* block-size (mod transaction-count 2))
            transaction-count (+ transaction-count 1)
            db-state (assoc db-state :transaction-count transaction-count)
            db-state (assoc db-state :aamap aamap)
            db-state (assoc db-state :allocated *allocated*)
            db-state (assoc db-state :release-pending *release-pending*)
            ^ByteBuffer bb (ByteBuffer/allocate block-size)
            ^FileChannel file-channel (:db-file-channel opts)
            map-size (byte-length aamap)
            release-pending-map-size (byte-length *release-pending*)
            available-long-array (.toLongArray *allocated*)
            ala-len (alength available-long-array)
            available-size (* ala-len 8)]
        (if (< block-size (+ 4 4 4 4 8 map-size release-pending-map-size available-size 32))
          (throw (Exception. "block-size exceeded on write")))
        (.putInt bb block-size)
        (.putInt bb map-size)
        (.putInt bb release-pending-map-size)
        (.putInt bb ala-len)
        (.putLong bb transaction-count)
        (put-aa bb aamap)
        (put-aa bb *release-pending*)
        ;put array-----------------
        (put-cs256 bb (compute-cs256 (.flip (.duplicate bb))))
        (.flip bb)
        (.write file-channel bb (long position))
        db-state)
      (catch Exception e
        (.printStackTrace e)
        (throw e)))))
