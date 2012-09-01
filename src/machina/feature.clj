(ns machina.feature
  (:require [machina.attribute :as attr]
            [machina.data :as data]))

;;
;; Array fragment (dense and sparse) data structures
;;

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
       (combined-seq-helper (rest arr-frag-seq) (+ gidx (:length current)) 0)
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
           (combined-seq-helper (rest arr-frag-seq) (+ gidx (:length current)) 0)
           (lazy-seq (cons output-pair (combined-seq-helper next-seq gidx (inc lidx))))))))))

(defn combined-seq
  "Convert a sequence of ArrayFragments into a lazy sequence of
   (index, value) pairs.  Optionally takes an offset index for the first element."
  ([arr-frag-seq]
     (combined-seq arr-frag-seq 0))
  ([arr-frag-seq offset]
     (assert (sequential? arr-frag-seq) "Input must be a sequence of ArrayFragments")
     (combined-seq-helper arr-frag-seq offset 0)))

;;
;; Feature and Feature Set functions and structures
;;

(defrecord Feature [compute])

(defn feature
  [compute & other]
  (assoc (Feature. compute) :meta (apply hash-map other)))

(defn- get-attribute
  [feature]
  (-> feature :meta :attrs))

(defn- get-attributes
  [features]
  (map get-attribute features))

(defn attributes?
  [features]
  (some #(not (nil? (get-attribute %))) features))

(defn- get-compute-functions
  [features]
  (map :compute features))

(defprotocol DatapointProcessor
  (compute-item [this item])
  (compute-items [this items]))

(defn- auto-wrap-dense
  [coll-or-frag]
  (if (sequential? coll-or-frag)
    (dense-fragment coll-or-frag)
    coll-or-frag))

(defrecord FeatureSet [compute]
  DatapointProcessor
  (compute-item [this item] (combined-seq (map auto-wrap-dense ((:compute this) item))))
  (compute-items [this items] (map (partial compute-item this) items))
  )

(def ^{:dynamic true} *parallel* true)

(defn- build-feature-set
  [features]
  (let [mapf (if *parallel* pmap map)]
    (FeatureSet.
     (fn [item]
       (mapf #(% item) (get-compute-functions features))))))

(defn feature-set
  [features]
  (let [fs (build-feature-set features)]
    (if (attributes? features)
      (let [attrs (attr/merge-attributes (get-attributes features))]
        (assoc fs :attrs attrs))
      fs)))
