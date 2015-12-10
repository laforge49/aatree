(ns aatree.null-db-cache-trait
  (:require [aatree.core :refer :all]))

(defn null-db-cache [this]
  (-> this
      (assoc
        :block-read
        (fn [this block-nbr byte-buffer]
          (db-file-read this byte-buffer (* block-nbr (db-block-size this)))))
      (assoc
        :block-write
        (fn [this block-nbr byte-buffer]
          (db-file-write this byte-buffer (* block-nbr (db-block-size this)))))
      (assoc
        :block-clear
        (fn [this block-nbr]))
      ))
