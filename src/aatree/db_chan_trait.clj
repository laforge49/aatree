(ns aatree.db-chan-trait
  (:require [clojure.core.async :refer [>! <! >!! <!! go chan buffer close! thread
                                        alts! alts!! alt! alt!! timeout]]
            [aatree.core :refer :all]
            [aatree.closer-trait :refer :all]
            [clojure.tools.logging :as log]))

(set! *warn-on-reflection* true)

(defn- db-vstate-set! [db, new-db-update-state]
  (vswap!
    (:db-update-vstate db)
    (fn [db-update-state]
      (if db-update-state
        (let [e (Exception. "db-update-vstate not nil")]
          (log/warn e "db update failure")
          (throw e)))
      new-db-update-state)))

(defn- db-vstate-clear! [db]
  (vswap!
    (:db-update-vstate db)
    (fn [db-update-state]
      (if (not db-update-state)
        (let [e (Exception. "db-update-vstate nil")]
          (log/warn e "db update failure")
          (throw e)))
      nil)))

(defn- process-chan [this]
  (let [db-state-atom (:db-state-atom this)
        db-chan (:db-chan this)]
    (loop []
      (when-let [msg (<!! db-chan)]
        msg
        (let [[app-updater rchan] msg]
          ((:db-updater this) this app-updater)
          (when rchan
            (>!! rchan true)))
        (recur)))))

(defn db-chan [this]
  (-> this
      (assoc
        :db-update-vstate
        (volatile! nil))
      (assoc
        :create-db-chan
        (fn [db initial-state]
          (let [db (-> db
              (assoc
                :db-state-atom
                (atom initial-state))
              (assoc
                :db-chan
                (chan (:db-buf-or-n db)))
              (open-component
                "db-chan"
                (fn [d] (close! (:db-chan d)))))]
            (thread (process-chan db))
            db)))
      (assoc
        :db-get-state
        (fn [db]
          @(:db-state-atom db)))
      (assoc
        :db-send
        (fn [db app-updater]
          (>!! (:db-chan db) [app-updater nil])))
      (assoc
        :db-update
        (fn [db app-updater]
          (let [rchan (chan)
                _ (>!! (:db-chan db) [app-updater rchan])
                send-update-timeout (:send-update-timeout db)
                rsp (if send-update-timeout
                      (first (alts!! [rchan (timeout send-update-timeout)]))
                      (<!! rchan))]
            (close! rchan))))))