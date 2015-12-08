(ns aatree.db-chan-test
  (:require [clojure.test :refer :all]
            [aatree.core :refer :all]
            [aatree.db-chan-trait :refer :all]
            [aatree.closer-trait :refer :all]))

(set! *warn-on-reflection* true)

(deftest chan-trait
  (try
    (println "#######################")
    (let [db (-> {:db-updater
                  (fn [this app-updater]
                    (app-updater this))}
                 (db-chan)
                 (create-db-chan {}))]
      (db-send db (fn [_] (println ":-)")))
      (db-send db (fn [_] (println ":D")))
      (println (db-update db (fn [_] (println ";-}"))))
      (let [db (assoc db :send-update-timeout 300)]
        (println (db-update db (fn [_] (Thread/sleep 1000))))
        (println (db-update db (fn [_] (Thread/sleep 1000))))
        (println (db-update db (fn [_] (throw (Exception. "fun")))))
        (Thread/sleep 200)
        (close-components db)))
    (finally
      (println "done"))))