(ns aatree.file-example
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)
           (java.io File)))

(set! *warn-on-reflection* true)

(def opts (lazy-opts))

(def bm1 (conj (new-sorted-map opts) {:dog "Jack" :cat "Sammy" :rabbit "Henry"}))
(def bm1-len (byte-length bm1))
(def ^ByteBuffer bb (ByteBuffer/allocate bm1-len))
(put-bytebuffer bm1 bb)
(.flip bb)
(file-save bb (File. "file-example.lazy"))

(let [^ByteBuffer bb (file-load (File. "file-example.lazy"))
      bm2 (load-sorted-map bb opts)]
  (println bm2))
