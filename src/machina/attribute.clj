(ns machina.attribute)

(defrecord Attribute [name type])

(defn validate
  [attribute dp]
  ((case (:type attribute)
      :numeric (fn [d] (number? d))
      :nominal (fn [d] (contains? (:vals attribute) d))
      :string  (fn [d] (string? d))) dp
   ))

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
