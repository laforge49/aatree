(ns aatree.core
  (:require [aatree.nodes :refer :all])
  (:require [aatree.lazy-nodes :refer :all])
  (:import (aatree AAMap AAVector AASet)
           (aatree.nodes FlexVector WrapperNode)
           (clojure.lang RT)
           (java.io File)
           (java.nio ByteBuffer LongBuffer)
           (java.nio.file StandardOpenOption OpenOption)
           (java.nio.channels FileChannel)
           (java.util BitSet)))

(set! *warn-on-reflection* true)

(defn addn [^FlexVector vec ndx val]
  (.addNode vec ndx val))

(defn dropn [vec & args]
  (reduce (fn [^FlexVector v i] (.dropNode v i)) vec args))

(defn load-vector [buffer opts]
  ((:load-vector opts) buffer opts))

(defn load-sorted-map [buffer opts]
  ((:load-sorted-map opts) buffer opts))

(defn load-sorted-set [buffer opts]
  ((:load-sorted-set opts) buffer opts))

(defn byte-length [noded]
  (node-byte-length (get-inode noded) (get-opts noded)))

(defn put-aa [buffer aa]
  (lazy-write (get-inode aa) buffer (get-opts aa)))

(defn has-aafactories [opts] (:new-sorted-map opts))

(defn standard-opts
  ([] (standard-opts {}))
  ([opts]
   (-> opts
       (assoc :new-sorted-map
              (fn [o]
                (let [c (:comparator o)]
                  (if c
                    (sorted-map-by c)
                    (sorted-map)))))
       (assoc :new-vector
              (fn [o] []))
       (assoc :new-sorted-set
              (fn [o]
                (let [c (:comparator o)]
                  (if c
                    (sorted-set-by c)
                    (sorted-set))))))))

(defn basic-opts
  ([] (basic-opts {}))
  ([opts]
   (-> opts
       (assoc :new-sorted-map
              (fn [o]
                (if (:coparator opts)
                  (new AAMap emptyNode o)
                  (new AAMap
                       emptyNode
                       (assoc o :comparator RT/DEFAULT_COMPARATOR)))))
       (assoc :new-vector
              (fn [o] (new AAVector emptyNode o)))
       (assoc :new-sorted-set
              (fn [o]
                (let [mpl
                      (if (:coparator o)
                        (new AAMap emptyNode o)
                        (new AAMap emptyNode (assoc
                                               o
                                               :comparator
                                               RT/DEFAULT_COMPARATOR)))]
                  (new AASet mpl)))))))

(defn lazy-opts
  ([] (lazy-opts {}))
  ([opts]
   (-> opts
       (assoc :node-read lazy-read)
       (assoc :load-vector load-lazy-vector)
       (assoc :load-sorted-map load-lazy-sorted-map)
       (assoc :load-sorted-set load-lazy-sorted-set)
       (assoc :new-sorted-map
              (fn [r]
                (let [r (if (:comparator r)
                          r
                          (assoc r :comparator RT/DEFAULT_COMPARATOR))
                      r (if (:factory-registry r)
                          r
                          (assoc r :factory-registry default-factory-registry))
                      r (map-opts r)]
                  (new AAMap emptyLazyNode r))))
       (assoc :new-vector
              (fn [o]
                (if (:factory-registry o)
                  (new AAVector emptyLazyNode (vector-opts o))
                  (new AAVector
                       emptyLazyNode
                       (vector-opts (assoc o :factory-registry default-factory-registry))))))
       (assoc :new-sorted-set
              (fn [o]
                (let [r o
                      r (if (:comparator r)
                          r
                          (assoc r :comparator RT/DEFAULT_COMPARATOR))
                      r (if (:factory-registry r)
                          r
                          (assoc r :factory-registry default-factory-registry))
                      r (set-opts r)]
                  (new AASet
                       (new AAMap emptyLazyNode r))))))))

(defn new-sorted-map [opts]
  ((:new-sorted-map opts) opts))

(defn new-vector [opts]
  ((:new-vector opts) opts))

(defn new-sorted-set [opts]
  ((:new-sorted-set opts) opts))

(defn file-save [^ByteBuffer buffer ^File file]
  (let [^FileChannel fc (FileChannel/open (.toPath file)
                                          (into-array OpenOption
                                                      [StandardOpenOption/CREATE
                                                       StandardOpenOption/TRUNCATE_EXISTING
                                                       StandardOpenOption/WRITE]))]
    (try
      (.write fc buffer)
      (catch Exception e
        (.close fc)
        (throw e)))
    (.close fc)))

(defn ^ByteBuffer file-load [^File file]
  (let [^FileChannel fc (FileChannel/open (.toPath file)
                                          (into-array OpenOption
                                                      [StandardOpenOption/CREATE
                                                       StandardOpenOption/READ]))]
    (try
      (let [size (.size fc)
            bb (ByteBuffer/allocate size)]
        (.read fc bb)
        (.flip bb)
        bb)
      (finally
        (.close fc)))))

(defn ^BitSet compute-cs256 [^ByteBuffer bb]
  (let [^BitSet bs (BitSet. 256)
        len (.remaining bb)]
    (reduce (fn [^BitSet bitset i]
              (let [bbv (- (.get bb) Byte/MIN_VALUE)
                    j (mod (+ bbv (* i 7)) 256)]
                (.flip bitset j))
              bitset)
              bs
              (range len))
    bs))

(defn put-cs256 [^ByteBuffer bb ^BitSet cs256]
  (let [la (.toLongArray cs256)
        lal (alength la)
        r (range (- 4 lal))
        ^LongBuffer lb (.asLongBuffer bb)]
    (.put lb la)
    (reduce (fn [a b] (.put lb 0)) 0 r))
  (.position bb (+ (.position bb) 32)))

(defn ^BitSet get-cs256 [^ByteBuffer bb]
  (let [la (long-array 4)
        _ (.get (.asLongBuffer bb) (longs la))
        bs (BitSet/valueOf (longs la))]
    (.position bb (+ (.position bb) 32))
    bs))

(defn db-close [opts] ((:db-close opts) opts))

(defn db-get-sorted-map [opts] ((:db-get-sorted-map opts) opts))

(defn db-transaction-count [opts] ((:db-transaction-count opts) opts))

(defn db-send [app-updater opts] ((:db-send opts) app-updater opts))

(defn db-update [app-updater opts] ((:db-update opts) app-updater opts))

(defn db-allocated [opts] ((:db-allocated opts) opts))

(defn db-allocate [opts] ((:db-allocate opts) opts))

(defn db-release-pending [opts] ((:db-release-pending opts) opts))

(defn db-release [block-position opts] ((:db-release opts) block-position opts))

(defn db-process-pending [age trans opts] ((:db-process-pending opts) age trans opts))