(ns machina.export
  (require [machina.feature :as fs]
           [machina.attribute :as attr]
           [clojure.string :as str]))

(defrecord ClassData [attr func])

(defn class-data
  [class-values class-function]
  (ClassData. (attr/nominal "class" class-values) class-function))

(def ^{:dynamic true} *arff-mode* :dense)

(def ^{:dynamic true} *parallel* true)

(defprotocol OutputStringWriter
  (write [this string]))

(defn sync-java-writer
  [^java.io.Writer writer]
  (reify
    OutputStringWriter
      (write [this string]
        (locking writer
          (.write writer string)))))

;;
;; WEKA
;;

(defn- escape
  [obj]
  (if (string? obj)
    (str "\"" obj "\"")
    obj))

(defn- format-set-vals
  [set-attr]
  (str/join "," (map escape (:vals set-attr))))

(defn- weka-header
  [attribute]
  (str "@attribute "
       (:name attribute)
       " "
       (case (:type attribute)
         :numeric "numeric"
         :string "string"
         :nominal (format "{%s}" (format-set-vals attribute)))
       "\n"))

(defn- weka-dense-line
  [feature-vector class-lbl]
  (let [feature-values (map second feature-vector)]
    (str
     (str/join "," (map escape feature-values))
     ","
     (escape class-lbl)
     "\n")))

(defn- weka-sparse-line
  [feature-vector class-lbl]
  (str
   "{"
   (str/join ", " (map #(format "%s %s" (first %) (escape (second %))) (filter #(not= 0 (second %)) feature-vector)))
   ", "
   (format "%s %s" (count feature-vector) (escape class-lbl))
   "}\n"))

(defn weka-arff
  ([data-points feature-set class-data writer]
     (weka-arff data-points feature-set class-data writer "default-relation"))
  ([data-points feature-set class-data writer relation-name]
     (assert (:attrs feature-set) "Feature set attributes are required for WEKA export")
     (write writer (str "@relation " relation-name "\n\n"))
     (doall (map #(write writer (weka-header %)) (:attrs feature-set)))
     (write writer (weka-header (:attr class-data)))
     (write writer "\n@data\n")
     (let [mapf (if *parallel* pmap map)
           mode *arff-mode*]
       (doall
        (mapf
         (fn [dp]
           (let [feature-vector (fs/compute-item feature-set dp)
                 class-value ((:func class-data) dp)]
             (write writer
              (str
               (case mode
                 :dense (weka-dense-line feature-vector class-value)
                 :sparse (weka-sparse-line feature-vector class-value))))))
         data-points)))))
