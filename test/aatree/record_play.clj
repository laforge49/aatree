(ns aatree.record-play)

(set! *warn-on-reflection* true)

(defrecord base [])

(defn new-base [opts]
  (let [b (->base)
        b (into b opts)
        b (assoc b :blap (fn [this] 42))]
    b))

(defrecord wackel [])

(defn new-wackel [opts]
  (let [w (->wackel)
        w (into w opts)
        w (assoc w :blip (fn [this x y z] (+ x y z)))]
    w))

(defprotocol gran
  (blip [this x y z])
  (blap [this]))

(def w (-> {} new-base new-wackel))

(extend-type wackel
  gran
  (blip [this x y z]
    ((:blip this) this x y z))
  (blap [this]
    ((:blap this) this)))

(def w (-> {} new-base new-wackel))

(println (blip w 1 2 3))                                    ; -> 6

(println (blap w))                                          ; -> 42
