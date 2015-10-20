(ns aatree.calf-test
  (:require [aatree.core :refer :all])
  (:require [aatree.calf :refer :all])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(def opts (calf-open (File. "file-example.lazy") 1000))

(println opts)

(db-close opts)
