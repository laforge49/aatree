(ns aatree.db-agent-trait
  (:require [aatree.core :refer :all])
  (:import (clojure.lang Agent)))

(set! *warn-on-reflection* true)

(defn db-agent [this]
  (-> this
      (assoc
        :create-db-chan
        (fn [db initial-state]
          (assoc
            db
            :db-agent
            (apply agent (initial-state db) (get db :db-agent-options [])))))
      (assoc
        :db-send
        (fn [db app-updater]
          (let [^Agent db-agent (:db-agent db)]
            (send-off db-agent (:db-updater db) db app-updater))))
      (assoc
        :db-update
        (fn [db app-updater]
          (db-send db app-updater)
          (let [send-write-timeout (:send-update-timeout db)
                db-agent (:db-agent db)]
            (if send-write-timeout
              (await-for send-write-timeout db-agent)
              (await db-agent)))))))