(ns aatree.null-db-cache-trait
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)))

(defn- check-size [this ^ByteBuffer byte-buffer]
  (let [db-block-size (db-block-size this)
        limit (.limit byte-buffer)]
    (if (> limit db-block-size)
      (throw (Exception. (str "byte buffer is too big:" limit))))))

(defn null-db-cache [this]
  (-> this
      (assoc
        :block-read
        (fn [this block-nbr ^ByteBuffer byte-buffer]
          (check-size this byte-buffer)
          (db-file-read this byte-buffer (* block-nbr (db-block-size this)))))
      (assoc
        :block-write
        (fn [this block-nbr ^ByteBuffer byte-buffer]
          (check-size this byte-buffer)
          (db-file-write this byte-buffer (* block-nbr (db-block-size this)))))
      (assoc
        :block-clear
        (fn [this block-nbr]))
      ))
