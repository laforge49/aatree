(ns aatree.lazy-set-examples
  (:require [aatree.core :refer :all])
  (:import (java.nio ByteBuffer)))

(set! *warn-on-reflection* true)

(def opts (lazy-opts))

(def empty-set (new-set opts))
(println (lazy-byte-length empty-set)); -> 1

(def ls1 (conj empty-set :dog :cat :rabbit))
(println ls1); -> #{:cat :dog :rabbit}

(def ls1-len (lazy-byte-length ls1))
(println ls1-len); -> 85

(def ^ByteBuffer bb (ByteBuffer/allocate ls1-len))
(lazy-write ls1 bb)
(.flip bb)
(def ls2 (load-aaset bb))
(println ls2); -> #{:cat :dog :rabbit}
