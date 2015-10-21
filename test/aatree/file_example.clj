(ns aatree.file-example
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)
           (java.io File)))

(set! *warn-on-reflection* true)

(def opts (lazy-opts))
(def empty-map (new-sorted-map opts))

(let [bm1 (conj empty-map {:dog "Jack" :cat "Sammy" :rabbit "Henry"})
      bm1-len (byte-length bm1)
      ^ByteBuffer bb (ByteBuffer/allocate bm1-len)]
  (put-aa bb bm1)
  (.flip bb)
  (file-save bb (File. "file-example.lazy")))

(let [^ByteBuffer bb (file-load (File. "file-example.lazy"))
      bm2 (load-sorted-map bb opts)]
  (println bm2)); -> {:cat Sammy, :dog Jack, :rabbit Henry}
