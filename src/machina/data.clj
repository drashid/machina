(ns machina.data)

(defrecord ArrayFragment [data length type])

(defn dense-fragment
  ([array-data]
     (dense-fragment array-data (count array-data)))
  ([array-data length]
     (assert (sequential? array-data))
     (assert (not (neg? length)))
     (ArrayFragment. array-data length :dense)))

(defn sparse-fragment
   [sparse-data length]
   (assert (map? sparse-data))
   (assert (not (neg? length)))
   (ArrayFragment. sparse-data length :sparse))

(defn combined-seq
  "Convert a sequence of ArrayFragments into a lazy sequence of
   (index, value) pairs."
  ([arr-frag-seq]
   (combined-seq arr-frag-seq 0 0))
  ([arr-frag-seq gidx]
   (combined-seq arr-frag-seq gidx 0))
  ([arr-frag-seq gidx lidx]
   (let [current (first arr-frag-seq)]
    (if (not (empty? arr-frag-seq))
      (if (and current (<= (:length current) lidx))
        (lazy-seq (combined-seq (rest arr-frag-seq) (+ gidx (:length current)) 0))
        (let [data-val (get (:data current) lidx 0)
              output-pair [(+ lidx gidx) data-val]]
          (lazy-seq (cons output-pair (combined-seq arr-frag-seq gidx (inc lidx))))))))))
