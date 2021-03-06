(ns aatree.null-db-cache-trait
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)))

(defn null-db-cache [this]
  (-> this
      (assoc
        :block-read
        (fn [db block-nbr block-length]
          (if (> block-length (db-block-size db))
            (throw (Exception. (str "block length is too big:" block-length))))
          (let [^ByteBuffer byte-buffer (ByteBuffer/allocate block-length)]
            (db-file-read db byte-buffer (* block-nbr (db-block-size db)))
            (.flip byte-buffer)
            byte-buffer)))
      (assoc
        :block-write
        (fn [db block-nbr ^ByteBuffer byte-buffer]
          (check-buffer-size db byte-buffer)
          (db-file-write db byte-buffer (* block-nbr (db-block-size db)))))
      (assoc
        :block-clear
        (fn [db block-nbr]))
      ))
