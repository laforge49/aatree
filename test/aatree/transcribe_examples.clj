(ns aatree.transcribe-examples
  (:require [aatree.core :refer :all]))

(set! *warn-on-reflection* true)

; The recognized classes
(println (class []))
; -> clojure.lang.PersistentVector
(println (class (new-vector (standard-opts))))
; -> clojure.lang.PersistentVector
(println (class (new-vector (basic-opts))))
; -> aatree.AAVector
(println (class (sorted-map)))
; -> clojure.lang.PersistentTreeMap
(println (class (new-sorted-map (standard-opts))))
; -> clojure.lang.PersistentTreeMap
(println (class (new-sorted-map (basic-opts))))
; -> clojure.lang.aatree.AAMap
(println (class (sorted-set)))
; -> clojure.lang.PersistentTreeSet
(println (class (new-sorted-set (standard-opts))))
; -> clojure.lang.PersistentTreeSet
(println (class (new-sorted-set (basic-opts))))
; -> clojure.lang.aatree.AASet
(println)

; Top-level conversion
(println (class (transcribe [] (basic-opts))))
; -> aatree.AAVector
(println (class (transcribe (transcribe [] (basic-opts)) (standard-opts))))
; -> clojure.lang.PersistentVector
(println (class (transcribe (sorted-map) (basic-opts))))
; -> clojure.lang.aatree.AAMap
(println (class (transcribe (transcribe (sorted-map) (basic-opts)) (standard-opts))))
; -> clojure.lang.PersistentTreeMap
(println (class (transcribe (sorted-set) (basic-opts))))
; -> clojure.lang.aatree.AASet
(println (class (transcribe (transcribe (sorted-set) (basic-opts)) (standard-opts))))
; -> clojure.lang.PersistentTreeSet
(println)

; No conversion of unrecognized structures
(println (class {}))
; -> clojure.lang.PersistentArrayMap
(println (class (transcribe {} (basic-opts))))
; -> clojure.lang.PersistentArrayMap
(println (class (list)))
; -> clojure.lang.PersistentList$EmptyList
(println (class (transcribe (list) (basic-opts))))
; -> clojure.lang.PersistentList$EmptyList
(println)

; recursive conversion of recognized structures
(def std-vec [[]])
(def basic-vec (transcribe std-vec (basic-opts)))
(println (class (basic-vec 0)))
; -> aatree.AAVector
(println (class (transcribe basic-vec (standard-opts))))
; -> clojure.lang.PersistentVector
(def std-map (conj (sorted-map) [:m (sorted-map)]))
(def basic-map (transcribe std-map (basic-opts)))
(println (class (basic-map :m)))
; -> aatree.AAMap
(println (class (transcribe basic-map (standard-opts))))
; -> clojure.lang.PersistentTreeMap
