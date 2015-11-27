(ns aatree.calf
  (:require [aatree.core :refer :all]
            [aatree.nodes :refer :all]
            [aatree.db-file-trait :refer :all]
            [aatree.db-agent-trait :refer :all])
  (:import (java.io File)
           (java.nio ByteBuffer)))

(set! *warn-on-reflection* true)

(defn- calf-updater [db-state this app-updater]
  (try
    (let [aamap (app-updater this (:aamap db-state))
          transaction-count (:transaction-count db-state)
          block-size (:db-block-size this)
          position (* block-size (mod transaction-count 2))
          transaction-count (+ transaction-count 1)
          db-state (assoc db-state :transaction-count transaction-count)
          db-state (assoc db-state :aamap aamap)
          ^ByteBuffer bb (ByteBuffer/allocate block-size)
          map-size (byte-length aamap)]
      (if (< block-size (+ 4 4 8 map-size 32))
        (throw (Exception. "block-size exceeded on write")))
      (.putInt bb block-size)
      (.putInt bb map-size)
      (.putLong bb transaction-count)
      (put-aa bb aamap)
      (put-cs256 bb (compute-cs256 (.flip (.duplicate bb))))
      (.flip bb)
      (db-file-write-root this bb (long position))
      db-state)
    (catch Exception e
      (.printStackTrace e)
      (throw e))))

(defn calf-null-updater [this aamap]
  aamap)

(defn- calf-new [this]
  (let [db-state {:transaction-count 0 :aamap (new-sorted-map this)}
        db-state (calf-updater db-state this calf-null-updater)
        db-state (calf-updater db-state this calf-null-updater)]
    db-state))

(defn- calf-read [this position]
  (let [block-size (:db-block-size this)
        ^ByteBuffer bb (ByteBuffer/allocate block-size)
        _ (.limit bb (+ 4 4 8))
        _ (db-file-read this bb (long position))
        _ (.flip bb)]
    (if (not= block-size (.getInt bb))
      nil
      (let [map-size (.getInt bb)
            _ (if (< block-size (+ 4 4 8 map-size 32))
                (throw (Exception. "block-size exceeded on read")))
            transaction-count (.getLong bb)
            input-size (+ (.limit bb) map-size 32)
            _ (.limit bb input-size)
            _ (db-file-read this bb (long (+ position 4 4 8)))
            _ (.flip bb)
            csp (- input-size 32)
            _ (.limit bb csp)
            cs (compute-cs256 bb)
            _ (.limit bb input-size)
            ocs (get-cs256 bb)
            _ (.position bb (+ 4 4 8))
            _ (.limit bb csp)
            aamap (load-sorted-map bb this)]
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

(defn- calf-old [this]
  (let [block-size (:db-block-size this)
        state0 (calf-read this 0)
        state1 (calf-read this block-size)]
    (choose state0 state1)))

(defn- create-initial-state [this]
  (choice this db-file-empty? calf-new calf-old))

(defn calf-open
  ([file block-size] (calf-open {} file block-size))
  ([this ^File file block-size]
   (let [this (-> this
                  (db-file-open file)
                  (assoc :db-block-size block-size)
                  (default :new-sorted-map lazy-opts)
                  (default :create-db-chan db-agent)
                  (assoc
                    :db-get-sorted-map
                    (fn [calf] (:aamap @(:db-agent calf))))
                  (assoc
                    :db-transaction-count
                    (fn [calf] (:transaction-count @(:db-agent calf))))
                  (assoc :db-updater calf-updater))]
     (create-db-chan this create-initial-state))))
