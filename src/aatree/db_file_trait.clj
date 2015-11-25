(ns aatree.db-file-trait
  (:require [aatree.closer-trait :refer :all])
  (:import (java.nio.channels FileChannel)
           (java.nio.file OpenOption StandardOpenOption)
           (java.io File)))

(set! *warn-on-reflection* true)

(defn db-file-open
  ([this file]
   (db-file-open (assoc this :db-file file)))
  ([this]
   (if (:db-file-channel this)
     this
     (do
       (if (not (:db-file this))
         (throw (Exception. "missing :db-file option")))
       (let [^File file (:db-file this)
             ^FileChannel file-channel
             (FileChannel/open (.toPath file)
                               (into-array OpenOption
                                           [StandardOpenOption/CREATE
                                            StandardOpenOption/READ
                                            StandardOpenOption/WRITE]))
             this (assoc this :db-file-channel file-channel)
             this (on-close this (fn [_] (.close file-channel)) (str "file " file))
             this (assoc this
                    :db-file-empty?
                    (fn []
                      (= 0 (.size file-channel))))
             this (assoc this
                    :db-file-read
                    (fn [byte-buffer position]
                      (.read file-channel byte-buffer position)))
             this (assoc this
                    :db-file-write
                    (fn [byte-buffer position]
                      (.write file-channel byte-buffer position)))
             this (assoc this
                    :db-file-write-root
                    (fn [byte-buffer position]
                      (.force file-channel true)
                      (.write file-channel byte-buffer position)
                      (.force file-channel true)))
             this (assoc this
                    :db-file-force
                    (fn []
                      (.force file-channel true)))]
       this)))))
