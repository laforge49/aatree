(ns aatree.yearling
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)
           (java.nio.channels FileChannel)
           (java.util BitSet)
           (clojure.lang Agent)))


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
        uber-map (assoc uber-map :release-pending (new-sorted-map opts))
        uber-map (assoc uber-map :app-map (new-sorted-map opts))
        db-state {:transaction-count 0 :uber-map uber-map :allocated (BitSet.)}
        opts (create-db-agent db-state opts)]
    (calf-update yearling-null-updater opts)
    (calf-update yearling-null-updater opts)
    opts))
