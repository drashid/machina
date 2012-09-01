(ns machina.export
  (require [machina.feature :as fs]
           [machina.attribute :as attr]
           [clojure.string :as str]))

;;
;; Class
;;

(defrecord ClassData [attr func])

(defn class-data
  ([class-function]
     (ClassData. (attr/numeric "class") class-function))
  ([class-values class-function]
     (ClassData. (attr/nominal "class" class-values) class-function)))

(defn binary-class-data
  [class-values class-function]
  (assert (= 2 (count class-values)) "Binary classes must have two possible values")
  (class-data class-values class-function))

;;
;; IO Helper
;;

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

(def ^{:dynamic true} *arff-mode* :dense)

(def ^{:dynamic true} *parallel* true)

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

;;
;; Svm Light
;;


(defn- svm-light-class
  [class-attr cls-val]
  (case (:type class-attr)
    ; Convert class label such as 'SPAM'/'HAM' to SVM-Light format of -1/1 -- TODO: 0?
    :nominal
    (let [other (first (disj (:vals class-attr) cls-val))]
      (compare cls-val other))
    ; Let numerics go through as svm-light also works with ranking
    :numeric
    cls-val
    ))

(defn- svm-light-line
  [feature-vector class-lbl]
  (str class-lbl
       " "
       ;; index is incremented by 1 as per svm-light format (indexes start at 1, 0 is an internal bias term)
       (str/join " " (map #(format "%s:%s" (inc (first %)) (second %))
                          (filter #(and (not= 0 (second %)) (number? (second %))) feature-vector)))
       "\n"))

(defn svm-light
  ([data-points feature-set writer]
     (let [zero-class-data (class-data (fn [dp] 0))]
       (svm-light data-points feature-set zero-class-data writer)))
  ([data-points feature-set label-data writer]
     (let [mapf (if *parallel* pmap map)]
       (doall
        (mapf
         (fn [dp]
           (let [feature-vector (fs/compute-item feature-set dp)
                 class-value (svm-light-class
                              (:attr label-data)
                              ((:func label-data) dp))]
             (write writer (svm-light-line feature-vector class-value))
             ))
         data-points)))))
