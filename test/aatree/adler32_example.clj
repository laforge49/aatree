(ns aatree.adler32-example
  (:require [aatree.core :refer :all])
  (:import (java.util.zip Adler32)
           (java.nio ByteBuffer)
           (java.io File)))

(set! *warn-on-reflection* true)

(def opts (lazy-opts))
(def empty-vec (new-vector opts))
(def lv1 (conj empty-vec 1 2 3))

(def bb-len (+ (byte-length lv1) 8))
(def ^ByteBuffer bb (ByteBuffer/allocate bb-len))
(put-bytebuffer lv1 bb)
(def ^Adler32 adler32 (Adler32.))
(def ^ByteBuffer abb (.flip (.duplicate bb)))
(.update adler32 abb)
(.putLong bb (.getValue adler32))
(.flip bb)
(file-save bb (File. "adler32-example.lazy"))

(let [^ByteBuffer bb (file-load (File. "adler32-example.lazy"))
      lv2 (load-vector bb opts)]
  (println lv2)); -> [1 2 3]
