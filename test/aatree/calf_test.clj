(ns aatree.calf-test
  (:require [aatree.core :refer :all])
  (:require [aatree.calf :refer :all])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(def ^File calf-test (File. "calf-test.lazy"))
(.delete calf-test)

(def opts (calf-open calf-test 10000))
(println (calf-transaction-count opts)); -> 2
(def data (calf-get opts))
(println data); -> {}
(def data (assoc data :fun "Clojure"))
(println data); -> {:fun Clojure}
(calf-write data opts)
(def data (calf-get opts))
(println data); -> {:fun Clojure}
(db-close opts)

(def opts (calf-open calf-test 10000))
(comment (println (calf-transaction-count opts)); -> 2
(def data (calf-get opts))
(println data); -> {}
(db-close opts))

(shutdown-agents)
