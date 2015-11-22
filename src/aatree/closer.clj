(ns aatree.closer)

(set! *warn-on-reflection* true)

(defn get-close [f opts]
  (let [fvs-atom (:closer-fvs-atom opts)]
    (if fvs-atom
      (let [fvs @fvs-atom]
        (if fvs
          (some #(= f (first %)) fvs)
          nil))
      nil)))

(defn on-close [f opts]
  (let [fvs-atom (:closer-fvs-atom opts)
        opts (if fvs-atom
               (if (get-close f opts)
                 opts
                 (swap! fvs-atom conj [f (atom false)]))
               (assoc opts :closer-fvs-atom
                           (atom (list [f (atom false)]))))]
    opts))

(defn do-close [opts]
  (let [fvs-atom (:closer-fvs-atom opts)]
    (if (nil? fvs-atom)
      opts
      (let [fvs @fvs-atom]
        (if (nil? fvs)
          opts
          (let [fv (first fvs)]
            (if (compare-and-set! (second fv) false true)
              ((first fv) opts))
            (compare-and-set! fvs-atom fvs (next fvs))
            (recur opts)))))))
