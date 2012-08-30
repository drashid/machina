(ns machina.data)

(defrecord ArrayFragment [data length type])

(defn- dense?
  "A dense fragment is a sequential collection of items"
  [frag]
  (= :dense (:type frag)))

(defn- sparse?
  "A sparse fragment is a map from {int->item}"
  [frag]
  (= :sparse (:type frag)))

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

(defn- combined-seq-helper
  "Private helper which takes a global index as well as a local index into the current element."
  [arr-frag-seq gidx lidx]
  (let [current (first arr-frag-seq)]
    (if (and (not (empty? arr-frag-seq)) (not (nil? current)))
      (cond
       ; If we've reached the end of the current fragment, move on to the next one,
       ; incrementing the global index and reverting the local index to 0
       (<= (:length current) lidx)
       (lazy-seq (combined-seq-helper (rest arr-frag-seq) (+ gidx (:length current)) 0))
       ; If we're sparse, pull out our data value from the map or default to 0
       (sparse? current)
       (let [data-val (get (:data current) lidx 0)
             output-pair [(+ lidx gidx) data-val]]
         (lazy-seq (cons output-pair (combined-seq-helper arr-frag-seq gidx (inc lidx)))))
       ; If we're dense, to handle very long lists without an O(n) lookup, we're actually going to
       ; use the current value, then discard it and reposition the head of our :data to the next element
       ; the local index will then serve to compute the global output index, but not be used for lookup
       (dense? current)
       (let [data-val (first (:data current))
             next-seq (cons (assoc current :data (rest (:data current))) (rest arr-frag-seq))
             output-pair [(+ lidx gidx) data-val]]
         (if (empty? (:data current)) ; If we still have elements left, keeps us from adding a bunch of (index, nil) if the length is an overestimate
           (lazy-seq (combined-seq-helper (rest arr-frag-seq) (+ gidx (:length current)) 0))
           (lazy-seq (cons output-pair (combined-seq-helper next-seq gidx (inc lidx))))))))))


(defn combined-seq
  "Convert a sequence of ArrayFragments into a lazy sequence of
   (index, value) pairs.  Optionally takes an offset index for the first element."
  ([arr-frag-seq]
   (combined-seq-helper arr-frag-seq 0 0))
  ([arr-frag-seq offset]
   (combined-seq-helper arr-frag-seq offset 0)))
