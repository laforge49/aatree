(ns aatree.record-play
  (:require [aatree.record-play0 :refer :all]))

(set! *warn-on-reflection* true)

(defprotocol gran
  (blip [this x y z])
  (blap [this]))

(def w (-> {} new-base new-wackel))

(extend-type aatree.record_play0.wackel
  gran
  (blip [this x y z]
    ((:blip this) this x y z))
  (blap [this]
    ((:blap this) this)))

(def w (-> {} new-base new-wackel))

(println (blip w 1 2 3))                                    ; -> 6

(println (blap w))                                          ; -> 42
