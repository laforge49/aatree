(ns aatree.calf
  (:require [aatree.core :refer :all])
  (:import (java.nio.channels FileChannel)
           (java.io File)
           (java.nio.file OpenOption StandardOpenOption)))

(set! *warn-on-reflection* true)

(defn calf-open
  ([file block-size] (calf-open file block-size {}))
  ([^File file block-size opts]
   (if (:file-channel opts)
     opts
     (let [opts (assoc opts :file file)
           opts (assoc opts :block-size block-size)
           file-channel
           (FileChannel/open (.toPath file)
                             (into-array OpenOption
                                         [StandardOpenOption/CREATE
                                          StandardOpenOption/READ
                                          StandardOpenOption/WRITE
                                          StandardOpenOption/SYNC]))
           opts (assoc opts :file-channel file-channel)
           opts (if (has-aafactories opts)
                  opts
                  (lazy-opts opts))
           opts (assoc opts :root-header-size (+ 4 8 32))]
       opts))))
