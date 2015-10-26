(ns aatree.yearling
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)
           (java.nio.channels FileChannel)
           (java.util BitSet)))


(set! *warn-on-reflection* true)

(def ^:dynamic ^BitSet *allocated*)

(def ^:dynamic *release-pending*)


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
              position (* block-size (mod transaction-count 2))
              transaction-count (+ transaction-count 1)
              db-state (assoc db-state :transaction-count transaction-count)
              db-state (assoc db-state :uber-map uber-map)
              db-state (assoc db-state :allocated *allocated*)
              ^ByteBuffer bb (ByteBuffer/allocate block-size)
              ^FileChannel file-channel (:db-file-channel opts)
              map-size (byte-length uber-map)
              available-long-array (.toLongArray *allocated*)
              ala-len (alength available-long-array)
              available-size (* ala-len 8)]
          (if (< block-size (+ 4 4 4 8 map-size available-size 32))
            (throw (Exception. "block-size exceeded on write")))
          (.putInt bb block-size)
          (.putInt bb map-size)
          (.putInt bb ala-len)
          (.putLong bb transaction-count)
          (put-aa bb uber-map)
          (.put (.asLongBuffer bb) available-long-array)
          (.position bb (+ (.position bb) (* ala-len 8)))
          (put-cs256 bb (compute-cs256 (.flip (.duplicate bb))))
          (.flip bb)
          (.write file-channel bb (long position))
          db-state)
        (catch Exception e
          (.printStackTrace e)
          (throw e))))))
