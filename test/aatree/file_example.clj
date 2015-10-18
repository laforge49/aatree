(ns aatree.file-example
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)
           (java.nio.file Path StandardOpenOption Paths OpenOption)
           (java.nio.channels FileChannel)
           (java.net URI)
           (java.io File)))

(set! *warn-on-reflection* true)

(defn save-bytebuffer [^ByteBuffer buffer ^File file]
  (let [^FileChannel fc (FileChannel/open (.toPath file)
                                          (into-array OpenOption
                                                      [StandardOpenOption/CREATE
                                                       StandardOpenOption/TRUNCATE_EXISTING
                                                       StandardOpenOption/WRITE]))]
    (try
      (.write fc buffer)
      (catch Exception e
        (.close fc)
        (throw e)))
    (.close fc)))

(defn ^ByteBuffer load-bytebuffer [^File file]
  (let [^FileChannel fc (FileChannel/open (.toPath file)
                                          (into-array OpenOption
                                                      [StandardOpenOption/CREATE
                                                       StandardOpenOption/READ]))]
    (try
      (let [size (.size fc)
            bb (ByteBuffer/allocate size)]
      (.read fc bb)
      (.flip bb)
      bb)
      (finally
        (.close fc)))))

(def opts (lazy-opts))

(def bm1 (conj (new-sorted-map opts) {:dog "Jack" :cat "Sammy" :rabbit "Henry"}))
(def bm1-len (byte-length bm1))
(def ^ByteBuffer bb (ByteBuffer/allocate bm1-len))
(put-bytebuffer bm1 bb)
(.flip bb)
(save-bytebuffer bb (File. "file-example.lazy"))

(let [^ByteBuffer bb (load-bytebuffer (File. "file-example.lazy"))
      bm2 (load-sorted-map bb opts)]
  (println bm2))
