(ns aatree.cs256-example
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)
           (java.io File)))

(set! *warn-on-reflection* true)

(def opts (lazy-opts))
(def empty-set (new-sorted-set opts))

(let [ls1 (conj empty-set :dog :cat :rabbit)
      bb-len (+ (byte-length ls1) 32)
      ^ByteBuffer bb (ByteBuffer/allocate bb-len)
      _ (put-aa bb ls1)
      ^ByteBuffer csbb (.flip (.duplicate bb))
      cs (cs256 csbb)]
  (put-cs256 bb cs)
  (.flip bb)
  (file-save bb (File. "cs245-example.lazy")))

(let [^ByteBuffer bb (file-load (File. "cs245-example.lazy"))
      csp (- (.limit bb) 32)
      ^ByteBuffer csbb (.limit (.duplicate bb) csp)
      cs (cs256 csbb)
      ocs (get-cs256 (.position (.duplicate bb) csp))
      lv2 (if (= cs ocs)
            (load-sorted-set bb opts)
            (throw (java.lang.Exception. "Checksum does not match")))]
  (println lv2)); -> #{:cat :dog :rabbit}
