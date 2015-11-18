(ns aatree.unique-timestamp-test
  (:require [aatree.unique-timestamp :refer :all]))

(set! *warn-on-reflection* true)

(if (= (new-timestamp) (new-timestamp))
  (throw (Exception. "timestamps equal!")))

(time (reduce (fn [_ _] (new-timestamp))
              nil
              (range 10240))) ; Must take more than 10 millis!
; -> "Elapsed time: 12.8779 msecs"
