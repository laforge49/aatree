(ns aatree.db-file-trait
  (:require [aatree.closer-trait :refer :all]
            [aatree.core :refer :all])
  (:import (java.nio.channels FileChannel)
           (java.nio.file OpenOption StandardOpenOption)
           (java.io File)))

(set! *warn-on-reflection* true)

(defn db-file-open
  ([this file]
   (db-file-open (assoc this :db-file file)))
  ([this]
   (default
     this
     :db-file-channel
     (fn [_]
       (let [_ (required this :db-file)
             ^File file (:db-file this)
             ^FileChannel file-channel
             (FileChannel/open
               (.toPath file)
               (into-array OpenOption
                           [StandardOpenOption/CREATE
                            StandardOpenOption/READ
                            StandardOpenOption/WRITE]))]
         (-> this
             (assoc :db-file-channel file-channel)
             (open-component (str "db file " file) (fn [_] (.close file-channel)))
             (assoc :db-file-empty?
                    (fn []
                      (= 0 (.size file-channel))))
             (assoc :db-file-read
                    (fn [byte-buffer position]
                      (.read file-channel byte-buffer position)))
             (assoc :db-file-write
                    (fn [byte-buffer position]
                      (.write file-channel byte-buffer position)))
             (assoc :db-file-write-root
                    (fn [byte-buffer position]
                      (.force file-channel true)
                      (.write file-channel byte-buffer position)
                      (.force file-channel true)))
             (assoc :db-file-force
                    (fn [] (.force file-channel true)))))))))
