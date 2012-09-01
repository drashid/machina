(ns machina.feature
  (:require [machina.attribute :as attr]
            [machina.data :as data]))

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

(defrecord FeatureSet [compute]
  DatapointProcessor
  (compute-item [this item] (data/combined-seq ((:compute this) item)))
  (compute-items [this items] (map (partial compute-item this) items))
  )

(def ^{:dynamic true} *parallel-comp* true)

(defn- build-feature-set
  [features]
  (let [mapf (if *parallel-comp* pmap map)]
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
