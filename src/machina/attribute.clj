(ns machina.attribute)

(defrecord Attribute [name type])

(defn validate
  [attribute dp]
  ((case (:type attribute)
      :numeric #(number? %)
      :nominal #(contains? (:vals attribute) %)
      :string  #(string? %)) dp ))

(defn numeric
  [name]
  (Attribute. name :numeric))

(defn nominal
  [name vals]
  (let [val-set (if (set? vals) vals (set vals))]
    (assoc (Attribute. name :nominal) :vals val-set)))

(defn string
  [name]
  (Attribute. name :string))

; class is a reserved feature name
(defn- unique?
  ([coll]
     (unique? #{"class"} coll))
  ([seen coll]
     (cond
      (empty? coll) true
      (contains? seen (first coll)) false
      :else (unique? (conj seen (first coll)) (rest coll)))))

(defn merge-attributes
  [& attr-colls]
  (let [validate-names #(assert (unique? (map :name %)) "Attribute names must be unique!")
        all-attrs (flatten (into [] attr-colls))]
    (do
      (validate-names all-attrs)
      all-attrs)))
