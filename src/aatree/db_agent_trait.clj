(ns aatree.db-agent-trait
  (:require [aatree.core :refer :all]
            [clojure.tools.logging :as log])
  (:import (clojure.lang Agent)))

(set! *warn-on-reflection* true)

(defn- db-vstate-set! [db, new-db-update-state]
  (vswap!
    (:db-update-vstate db)
    (fn [db-update-state]
      (if db-update-state
        (throw (Exception. "db-update-vstate not nil")))
      new-db-update-state)))

(defn- db-vstate-clear! [db]
  (vswap!
    (:db-update-vstate db)
    (fn [db-update-state]
      (if (not db-update-state)
        (throw (Exception. "db-update-vstate nil")))
      nil)))

(defn db-agent [this]
  (-> this
      (assoc
        :db-update-vstate
        (volatile! nil))
      (assoc
        :create-db-chan
        (fn [db initial-state]
          (assoc
            db
            :db-agent
            (apply agent initial-state (get db :db-agent-options [])))))
      (assoc
        :db-get-state
        (fn [db]
          @(:db-agent db)))
      (assoc
        :db-send
        (fn [db app-updater]
          (let [^Agent db-agent (:db-agent db)]
            (send-off
              db-agent
              (fn [db-state]
                (try
                  (db-vstate-set! db db-state)
                  ((:db-updater db) db app-updater)
                  (let [db-state @(:db-update-vstate db)]
                    (db-vstate-clear! db)
                    db-state)
                  (catch Throwable t
                    (log/error t "db update failure")
                    (throw t))))))))
      (assoc
        :db-update
        (fn [db app-updater]
          (db-send db app-updater)
          (let [send-write-timeout (:send-update-timeout db)
                db-agent (:db-agent db)]
            (if send-write-timeout
              (await-for send-write-timeout db-agent)
              (await db-agent)))))))