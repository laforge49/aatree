(ns aatree.lru-db-cache-trait
  (:require [aatree.core :refer :all]
            [clojure.core.cache :refer :all])
  (:import (java.nio ByteBuffer)))

(defn lru-db-cache [this]
  (-> this
      (assoc
        :block-cache-atom
        (atom (lru-cache-factory {})))
      (assoc
        :block-read
        (fn [db block-nbr ^ByteBuffer block-length]
          (if (> block-length (db-block-size db))
            (throw (Exception. (str "block length is too big:" block-length))))
          (let [^ByteBuffer byte-buffer (ByteBuffer/allocate block-length)]
            (db-file-read db byte-buffer (* block-nbr (db-block-size db)))
            byte-buffer)))
      (assoc
        :block-write
        (fn [db block-nbr ^ByteBuffer byte-buffer]
          (check-buffer-size db byte-buffer)
          (swap!
            (:block-cache-atom db)
            assoc
            block-nbr
            byte-buffer)
          (db-file-write db byte-buffer (* block-nbr (db-block-size db)))))
      (assoc
        :block-clear
        (fn [db block-nbr]
          (swap!
            (:block-cache-atom db)
            (fn [old]
              (evict old block-nbr)))))))
