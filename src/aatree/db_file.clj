(ns aatree.db-file
  (:require [clojure.tools.logging :as log])
  (:import (java.nio.channels FileChannel)
           (java.io File)
           (java.nio.file OpenOption StandardOpenOption)
           (java.nio ByteBuffer)))

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
                (fn [_]
                  (= 0 (.size file-channel))))]
     opts)))
