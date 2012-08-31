(ns machina.attribute)

(defrecord Attribute [name type])

(defn validate
  [attribute dp]
  ((:validate attribute) dp))

(defn numeric
  [name]
  (assoc (Attribute. name :numeric)
    :validate (fn [dp] (number? dp))))

(defn nominal
  [name vals]
  (let [val-set (if (set? vals) vals (set vals))]
    (assoc (Attribute. name :set)
      :vals val-set
      :validate (fn [dp] (contains? val-set dp)))))

(defn string
  [name]
  (assoc (Attribute. name :string)
    :validate (fn [dp] (string? dp))))
