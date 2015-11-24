(ns aatree.record-play0)

(set! *warn-on-reflection* true)

(defrecord base [])

(defn new-base [opts]
  (-> (->base)
      (into opts)
      (assoc :blap (fn [this] 42))))

(defrecord wackel [])

(defn new-wackel [opts]
  (-> (->wackel)
      (into opts)
      (assoc :blip (fn [this x y z] (+ x y z)))))
