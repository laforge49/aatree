(ns aatree.core
  (:require [aatree.nodes :refer :all]
            [aatree.lazy-nodes :refer :all]
            [aatree.virtual-nodes :refer :all]
            [medley.core :refer :all])
  (:import (aatree AAMap AAVector AASet)
           (aatree.nodes FlexVector INoded IFactory)
           (clojure.lang RT MapEntry)
           (java.io File)
           (java.nio ByteBuffer LongBuffer)
           (java.nio.file StandardOpenOption OpenOption)
           (java.nio.channels FileChannel)))

(set! *warn-on-reflection* true)

(defn default [map key f]
  (if (key map)
    map
    (f map)))

(defn choice [map cond fx fy]
  (if (cond map)
    (fx map)
    (fy map)))

(defn required [map key]
  (if (key map)
    map
    (throw (Exception. (str "missing entry: " key)))))

(defn assoc-default [map key val]
  (if (key map)
    map
    (assoc map key val)))

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
  (node-write (get-inode aa) buffer (get-opts aa)))

(defn has-aafactories [opts] (:new-sorted-map opts))

(defn new-standard-sorted-map [opts]
  (let [c (:comparator opts)]
    (if c
      (sorted-map-by c)
      (sorted-map))))

(defn new-standard-vector [opts] [])

(defn new-standard-sorted-set [opts]
  (let [c (:comparator opts)]
    (if c
      (sorted-set-by c)
      (sorted-set))))

(defn standard-opts
  ([] (standard-opts {}))
  ([opts]
   (-> opts
       (assoc :new-sorted-map new-standard-sorted-map)
       (assoc :new-vector new-standard-vector)
       (assoc :new-sorted-set new-standard-sorted-set))))

(defn new-basic-sorted-map [opts]
  (new AAMap emptyNode opts))

(defn new-basic-vector [opts]
  (new AAVector emptyNode opts))

(defn new-basic-sorted-set [opts]
  (new AASet (new AAMap emptyNode opts)))

(defn basic-opts
  ([] (basic-opts {}))
  ([opts]
   (-> opts
       (assoc-default :comparator RT/DEFAULT_COMPARATOR)
       (assoc :new-sorted-map new-basic-sorted-map)
       (assoc :new-vector new-basic-vector)
       (assoc :new-sorted-set new-basic-sorted-set))))

(defn new-lazy-sorted-map [opts]
  (new AAMap emptyLazyNode (map-opts opts)))

(defn new-lazy-vector [opts]
  (new AAVector emptyLazyNode (vector-opts opts)))

(defn new-lazy-sorted-set [opts]
  (new AASet (new AAMap emptyLazyNode (set-opts opts))))

(defn lazy-opts
  ([] (lazy-opts {}))
  ([opts]
   (-> opts
       (assoc-default :comparator RT/DEFAULT_COMPARATOR)
       (assoc-default :factory-registry default-factory-registry)
       (assoc :node-read lazy-read)
       (assoc :load-vector load-lazy-vector)
       (assoc :load-sorted-map load-lazy-sorted-map)
       (assoc :load-sorted-set load-lazy-sorted-set)
       (assoc :new-sorted-map new-lazy-sorted-map)
       (assoc :new-vector new-lazy-vector)
       (assoc :new-sorted-set new-lazy-sorted-set))))

(defn new-virtual-sorted-map [opts]
  (new AAMap emptyVirtualNode (map-opts opts)))

(defn new-virtual-vector [opts]
  (new AAVector emptyVirtualNode (vector-opts opts)))

(defn new-virtual-sorted-set [opts]
  (new AASet (new AAMap emptyVirtualNode (set-opts opts))))

(defn virtual-opts
  ([] (virtual-opts {}))
  ([opts]
   (-> opts
       (assoc-default :comparator RT/DEFAULT_COMPARATOR)
       (assoc-default :factory-registry default-factory-registry)
       (assoc :find-dropped-blocks find-dropped-blocks)
       (assoc :node-read virtual-read)
       (assoc :as-reference virtual-as-reference)
       (assoc :load-vector load-virtual-vector)
       (assoc :load-sorted-map load-virtual-sorted-map)
       (assoc :load-sorted-set load-virtual-sorted-set)
       (assoc :new-sorted-map new-virtual-sorted-map)
       (assoc :new-vector new-virtual-vector)
       (assoc :new-sorted-set new-virtual-sorted-set))))

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

(defn create-db-chan [this initial-state] ((:create-db-chan this) this initial-state))

(defn get-transaction-count [this]
  @(:transaction-count-atom this))

(defn get-last-node-id [this]
  @(:last-node-id-atom this))

(defn db-get-state
  ([this] ((:db-get-state this) this))
  ([this keys] (get-in (db-get-state this) keys)))

(defn update-get-in [this ks]
  (get-in @(:db-update-vstate this) ks))

(defn update-assoc-in [this ks v]
  (let [db-update-vstate (:db-update-vstate this)]
    (vreset! db-update-vstate (assoc-in @db-update-vstate ks v))))

(defn update-dissoc-in [this ks]
  (let [db-update-vstate (:db-update-vstate this)
        new-db-state (dissoc-in @db-update-vstate ks)]
    (vreset! db-update-vstate new-db-state)))

(defn db-send [this app-updater] ((:db-send this) this app-updater))

(defn db-update [this app-updater] ((:db-update this) this app-updater))

(defn db-file-empty? [this]
  ((:db-file-empty? this)))

(defn db-file-force [this]
  ((:db-file-force this)))

(defn db-file-read [this byte-buffer position]
  ((:db-file-read this) byte-buffer position))

(defn db-file-write [this byte-buffer position]
  ((:db-file-write this) byte-buffer position))

(defn db-file-write-root [this byte-buffer position]
  ((:db-file-write-root this) byte-buffer position))

(defn db-allocated [this] ((:db-allocated this) this))

(defn db-allocate [this] ((:db-allocate this) this))

(defn db-release-pending [this] ((:db-release-pending this) this))

(defn db-release [this block-position] ((:db-release this) this block-position))

(defn db-process-pending [this age trans] ((:db-process-pending this) this age trans))

(defn db-new-node-id [this] ((:db-new-node-id this) this))

(register-factory
  default-factory-registry
  vector-context
  (reify IFactory
    (factoryId [this] (byte \v))                            ;;;;;;;;;;;;;;;;;;;;;;;;;;; v aavector in aavector
    (instanceClass [this] aatree.AAVector)
    (qualified [this t2 opts] this)
    (valueLength [this node opts]
      (let [^INoded v (.getT2 node opts)]
        (node-byte-length (get-inode v) (get-opts v))))
    (deserialize [this node bb opts]
      ((:load-vector opts) bb opts))
    (writeValue [this node buffer opts]
      (let [^INoded v (.getT2 node opts)]
        (node-write (get-inode v) buffer (get-opts v))))
    (valueNode [this node opts]
      (let [^INoded v (.getT2 node opts)]
        (get-inode v)))))

(register-factory
  default-factory-registry
  vector-context
  (reify IFactory
    (factoryId [this] (byte \m))                            ;;;;;;;;;;;;;;;;;;;;;;;;;;; m aamap in aavector
    (instanceClass [this] aatree.AAMap)
    (qualified [this t2 opts] this)
    (valueLength [this node opts]
      (let [^INoded m (.getT2 node opts)]
        (node-byte-length (get-inode m) (get-opts m))))
    (deserialize [this node bb opts]
      ((:load-sorted-map opts) bb opts))
    (writeValue [this node buffer opts]
      (let [^INoded v (.getT2 node opts)]
        (node-write (get-inode v) buffer (get-opts v))))
    (valueNode [this node opts]
      (let [^INoded v (.getT2 node opts)]
        (get-inode v)))))

(register-factory
  default-factory-registry
  vector-context
  (reify IFactory
    (factoryId [this] (byte \s))                            ;;;;;;;;;;;;;;;;;;;;;;;;;;; s aaset in aavector
    (instanceClass [this] aatree.AASet)
    (qualified [this t2 opts] this)
    (valueLength [this node opts]
      (let [^INoded m (.getT2 node opts)]
        (node-byte-length (get-inode m) (get-opts m))))
    (deserialize [this node bb opts]
      ((:load-sorted-set opts) bb opts))
    (writeValue [this node buffer opts]
      (let [^INoded s (.getT2 node opts)]
        (node-write (get-inode s) buffer (get-opts s))))
    (valueNode [this node opts]
      (let [^INoded v (.getT2 node opts)]
        (get-inode v)))))

(register-factory
  default-factory-registry
  map-context
  (reify IFactory
    (factoryId [this] (byte \V))                            ;;;;;;;;;;;;;;;;;;;;;;;;;;; V aavector in aamap
    (instanceClass [this] aatree.AAVector)
    (qualified [this t2 opts] this)
    (sval [this inode opts]
      (key-sval this inode opts))
    (valueLength [this node opts]
      (let [^MapEntry map-entry (.getT2 node opts)
            ^INoded v (.getValue map-entry)]
        (+ (default-valueLength this node opts)
           (node-byte-length (get-inode v) (get-opts v)))))
    (deserialize [this node bb opts]
      (let [k (deserialize-sval this node bb opts)
            v ((:load-vector opts) bb opts)]
        (MapEntry. k v)))
    (writeValue [this node buffer opts]
      (default-write-value this node buffer opts)
      (let [^MapEntry map-entry (.getT2 node opts)
            ^INoded v (.getValue map-entry)]
        (node-write (get-inode v) buffer (get-opts v))))
    (valueNode [this node opts]
      (let [^MapEntry map-entry (.getT2 node opts)
            ^INoded v (.getValue map-entry)]
        (get-inode v)))))

(register-factory
  default-factory-registry
  map-context
  (reify IFactory
    (factoryId [this] (byte \M))                            ;;;;;;;;;;;;;;;;;;;;;;;;;;; M aamap in aamap
    (instanceClass [this] aatree.AAMap)
    (qualified [this t2 opts] this)
    (sval [this inode opts]
      (key-sval this inode opts))
    (valueLength [this node opts]
      (let [^MapEntry map-entry (.getT2 node opts)
            ^INoded m (.getValue map-entry)]
        (+ (default-valueLength this node opts)
           (node-byte-length (get-inode m) (get-opts m)))))
    (deserialize [this node bb opts]
      (let [k (deserialize-sval this node bb opts)
            v ((:load-sorted-map opts) bb opts)]
        (MapEntry. k v)))
    (writeValue [this node buffer opts]
      (default-write-value this node buffer opts)
      (let [^MapEntry map-entry (.getT2 node opts)
            ^INoded m (.getValue map-entry)]
        (node-write (get-inode m) buffer (get-opts m))))
    (valueNode [this node opts]
      (let [^MapEntry map-entry (.getT2 node opts)
            ^INoded v (.getValue map-entry)]
        (get-inode v)))))

(register-factory
  default-factory-registry
  map-context
  (reify IFactory
    (factoryId [this] (byte \S))                            ;;;;;;;;;;;;;;;;;;;;;;;;;;; S aaset in aamap
    (instanceClass [this] aatree.AASet)
    (qualified [this t2 opts] this)
    (sval [this inode opts]
      (key-sval this inode opts))
    (valueLength [this node opts]
      (let [^MapEntry map-entry (.getT2 node opts)
            ^INoded s (.getValue map-entry)]
        (+ (default-valueLength this node opts)
           (node-byte-length (get-inode s) (get-opts s)))))
    (deserialize [this node bb opts]
      (let [k (deserialize-sval this node bb opts)
            v ((:load-sorted-set opts) bb opts)]
        (MapEntry. k v)))
    (writeValue [this node buffer opts]
      (default-write-value this node buffer opts)
      (let [^MapEntry map-entry (.getT2 node opts)
            ^INoded s (.getValue map-entry)]
        (node-write (get-inode s) buffer (get-opts s))))
    (valueNode [this node opts]
      (let [^MapEntry map-entry (.getT2 node opts)
            ^INoded v (.getValue map-entry)]
        (get-inode v)))))

(defn transcribe [val opts] (transcriber val opts))

(defn aa-opts [aa] (get-opts aa))
