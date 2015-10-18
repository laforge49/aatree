(ns aatree.adler32-example
  (:require [aatree.core :refer :all])
  (:import (java.util.zip Adler32)
           (java.nio ByteBuffer)
           (java.io File)))

(set! *warn-on-reflection* true)

(def opts (lazy-opts))
(def empty-vec (new-vector opts))

(let [lv1 (conj empty-vec 1 2 3)
      bb-len (+ (byte-length lv1) 8)
      ^ByteBuffer bb (ByteBuffer/allocate bb-len)
      _ (put-bytebuffer lv1 bb)
      ^Adler32 adler32 (Adler32.)
      ^ByteBuffer abb (.flip (.duplicate bb))]
  (.update adler32 abb)
  (.putLong bb (.getValue adler32))
  (.flip bb)
  (file-save bb (File. "adler32-example.lazy")))

(let [^ByteBuffer bb (file-load (File. "adler32-example.lazy"))
      csp (- (.limit bb) 8)
      ^ByteBuffer abb (.limit (.duplicate bb) csp)
      ^Adler32 adler32 (Adler32.)
      _ (.update adler32 abb)
      cs (.getValue adler32)
      ocs (.getLong bb csp)
      lv2 (if (= cs ocs)
            (load-vector bb opts)
            (throw (java.lang.Exception. "Checksum does not match")))]
  (println lv2)); -> [1 2 3]
