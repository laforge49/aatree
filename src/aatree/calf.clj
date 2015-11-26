(ns aatree.calf
  (:require [aatree.core :refer :all]
            [aatree.nodes :refer :all]
            [aatree.db-file-trait :refer :all])
  (:import (java.io File)
           (java.nio ByteBuffer)
           (clojure.lang Agent)))

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

(defn- calf-send [this app-updater]
  (let [^Agent db-agent (:db-agent this)]
    (send-off db-agent calf-updater this app-updater)))

(defn calf-update [this app-updater]
  (db-send this app-updater)
  (let [send-write-timeout (:send-update-timeout this)
        db-agent (:db-agent this)]
    (if send-write-timeout
      (await-for send-write-timeout db-agent)
      (await db-agent))))

(defn- create-db-agent [this db-state]
  (assoc this :db-agent (apply agent db-state (get this :db-agent-options []))))

(defn calf-null-updater [this aamap]
  aamap)

(defn- calf-new [this]
  (let [db-state {:transaction-count 0 :aamap (new-sorted-map this)}
        this (create-db-agent this db-state)]
    (calf-update this calf-null-updater)
    (calf-update this calf-null-updater)
    this))

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
    (create-db-agent this (choose state0 state1))))

(defn- calf-transaction-count [this]
  (:transaction-count @(:db-agent this)))

(defn- calf-get-sorted-map [this]
  (:aamap @(:db-agent this)))

(defn calf-open
  ([file block-size] (calf-open {} file block-size))
  ([this ^File file block-size]
   (-> this
       (db-file-open file)
       (assoc :db-get-sorted-map calf-get-sorted-map)
       (assoc :db-transaction-count calf-transaction-count)
       (assoc :db-send calf-send)
       (assoc :db-update calf-update)
       (assoc :db-block-size block-size)
       (default :new-sorted-map lazy-opts)
       (choice db-file-empty? calf-new calf-old))))
