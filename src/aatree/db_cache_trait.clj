(ns aatree.db-cache-trait
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)))

(defn db-cache [this]
  (-> this
      (assoc
        :block-read
        (fn [this block-nbr ^ByteBuffer byte-buffer]
          (check-buffer-size this byte-buffer)
          (db-file-read this byte-buffer (* block-nbr (db-block-size this)))))
      (assoc
        :block-write
        (fn [this block-nbr ^ByteBuffer byte-buffer]
          (check-buffer-size this byte-buffer)
          (db-file-write this byte-buffer (* block-nbr (db-block-size this)))))
      (assoc
        :block-clear
        (fn [this block-nbr]))
      ))
