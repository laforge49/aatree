(ns aatree.unique-timestamp)

(set! *warn-on-reflection* true)

(def ^:private ts-atom (atom (bit-shift-left (System/currentTimeMillis) 10)))

(defn old-timestamp []
  @ts-atom)

(defn old-time-millis []
  (bit-shift-right (old-timestamp) 10))

(defn- new-time-millis [old-time-millis]
  (let [current-time-millis (System/currentTimeMillis)]
    (if (not= current-time-millis old-time-millis)
      current-time-millis
      (do
        (Thread/yield)
        (recur old-time-millis)))))

(defn new-timestamp []
  (swap! ts-atom
         (fn [old-timestamp]
           (let [current-time-millis (System/currentTimeMillis)
                 old-time-millis (bit-shift-right old-timestamp 10)
                 old-count (bit-and old-timestamp 1023)]
             (if (= current-time-millis old-time-millis)
               (if (= old-count 1023)
                 (bit-shift-left (new-time-millis old-time-millis) 10)
                 (+ old-timestamp 1))
               (bit-shift-left current-time-millis 10))))))