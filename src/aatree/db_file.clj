(ns aatree.db-file
  (:require [clojure.tools.logging :as log])
  (:import (java.nio.channels FileChannel)
           (java.nio.file OpenOption StandardOpenOption)))

(defn db-file-open
  ([file opts]
    (db-file-open (assoc opts :db-file file)))
  ([opts]
    (if (not (:db-file opts))
      (throw (Exception. "missing :db-file option")))
   (let [file (:db-file opts)
         ^FileChannel file-channel
         (FileChannel/open (.toPath file)
                           (into-array OpenOption
                                       [StandardOpenOption/CREATE
                                        StandardOpenOption/READ
                                        StandardOpenOption/WRITE]))
         opts (assoc opts :db-file-channel file-channel)
         opts (assoc opts
                :db-close
                (fn []
                  (try
                    (.close file-channel)
                    (catch Exception e
                      (log/warn e "exception on close of db-file")))))
         opts (assoc opts
                :db-file-empty?
                (fn []
                  (= 0 (.size file-channel))))
         opts (assoc opts
                :db-file-read
                (fn [byte-buffer position]
                  (.read file-channel byte-buffer position)))
         opts (assoc opts
                :db-file-write
                (fn [byte-buffer position]
                  (.write file-channel byte-buffer position)))
         opts (assoc opts
                :db-file-write-root
                (fn [byte-buffer position]
                  (.force file-channel true)
                  (.write file-channel byte-buffer position)
                  (.force file-channel true)))
         opts (assoc opts
                :db-file-force
                (fn []
                  (.force file-channel true)))
         ]
     opts)))
