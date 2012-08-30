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

(defn- get-index
  [array-frag index default]
  (let [getter (if (= (:type array-frag) :sparse) get nth)]
    (getter (:data array-frag) index default)))

(defn- combined-seq-helper
  "Private helper which takes a global index as well as a local index into the current element."
  [arr-frag-seq gidx lidx]
  (let [current (first arr-frag-seq)]
    (if (not (empty? arr-frag-seq))
      (if (and current (<= (:length current) lidx))
        (lazy-seq (combined-seq-helper (rest arr-frag-seq) (+ gidx (:length current)) 0))
        (let [data-val (get-index current lidx 0)
              output-pair [(+ lidx gidx) data-val]]
          (lazy-seq (cons output-pair (combined-seq-helper arr-frag-seq gidx (inc lidx)))))))))

(defn combined-seq
  "Convert a sequence of ArrayFragments into a lazy sequence of
   (index, value) pairs.  Optionally takes an offset index for the first element."
  ([arr-frag-seq]
   (combined-seq-helper arr-frag-seq 0 0))
  ([arr-frag-seq offset]
   (combined-seq-helper arr-frag-seq offset 0)))
