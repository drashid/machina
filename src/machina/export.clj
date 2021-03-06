(ns machina.export
  (require [machina.feature :as fs]
           [machina.attribute :as attr]
           [clojure.string :as str]))

;;
;; Class
;;

(def ^{:dynamic true} *class-name* "class")

(defrecord ClassData [attr func])

(defn class-data
  ([class-function]
     (ClassData. (attr/numeric *class-name*) class-function))
  ([class-values class-function]
     (ClassData. (attr/nominal *class-name* class-values) class-function)))

;;
;; IO Helper
;;

(defprotocol OutputStringWriter
  (write [this string])
  (close [this]))

(defn- sync-java-writer
  [^java.io.Writer writer]
  (reify
    OutputStringWriter
      (write [this string]
        (locking writer
          (.write writer string)))
      (close [this]
        (locking writer
          (.close writer)))))

(defn- create-agent-writer
  [writer]
  (agent writer))

(defn- agent-write
  [writer string]
  (.write writer string)
  writer)

(defn- a-write
  [writer string]
  (send-off writer agent-write string))

(defn- a-close
  [writer]
  (send writer #(.close %)))

;; Write generic exporter that simple calls callback functions for the given items.  Perhaps these can be rewritten to use that?

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
     {:pre [(:attrs feature-set)]}
     (let [out (sync-java-writer writer)]
       (try
         (write out (str "@relation " relation-name "\n\n"))
         (doall (map #(write out (weka-header %)) (:attrs feature-set)))
         (write out (weka-header (:attr class-data)))
         (write out "\n@data\n")
         (let [mapf (if *parallel* pmap map)
               mode *arff-mode*]
           (dorun
            (mapf
             (fn [dp]
               (let [feature-vector (fs/compute-item feature-set dp)
                     class-value ((:func class-data) dp)]
                 (write out (str
                   (case mode
                     :dense (weka-dense-line feature-vector class-value)
                     :sparse (weka-sparse-line feature-vector class-value))))))
             data-points)))
         (finally (close out))))))

;;
;; Svm Light
;;

(defn- svm-nominal-cls
  [class-attr cls-val]
  {:pre [(= 2 (count (:vals class-attr)))]}
  (let [other (first (disj (:vals class-attr) cls-val))]
    (compare cls-val other)))


(defn- svm-light-class
  [class-attr cls-val]
  (case (:type class-attr)
    ; Convert class label such as 'SPAM'/'HAM' to SVM-Light format of -1/1 -- TODO: 0?
    :nominal (svm-nominal-cls class-attr cls-val)

    ; Let numerics go through as svm-light also works with ranking
    :numeric cls-val))

(defn- svm-light-line
  [feature-vector class-lbl]
  (let [filter-non-numeric-and-zero (partial filter #(and (not= 0 (second %)) (number? (second %))))
        ;; index is incremented by 1 as per svm-light format (indexes start at 1, 0 is an internal bias term)
        format-index-and-value #(format "%s:%s" (inc (first %)) (second %))]
    (str class-lbl
         " "
         (str/join " " (map format-index-and-value (filter-non-numeric-and-zero feature-vector)))
         "\n")))

(defn svm-light
  ([data-points feature-set writer]
     (let [zero-class-data (class-data (fn [dp] 0))]
       (svm-light data-points feature-set zero-class-data writer)))
  ([data-points feature-set label-data writer]
     (let [mapf (if *parallel* pmap map)
           out (sync-java-writer writer)]
       (try
         (dorun
          (mapf
           (fn [dp]
             (let [feature-vector (fs/compute-item feature-set dp)
                   class-value (svm-light-class (:attr label-data) ((:func label-data) dp))]
               (write out (svm-light-line feature-vector class-value))))
           data-points))
         (finally (close out))))))
