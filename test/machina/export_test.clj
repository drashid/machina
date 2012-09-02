(ns machina.export-test
  (:require [machina.export :as export]
            [machina.attribute :as attr]
            [machina.feature :as fs]
            [clojure-csv.core :as csv]
            [clojure.java.io :as io])
  (:use clojure.test))

(def my-feature
  (fs/feature
   (fn [item]
     [1 2 3])
   :attrs [(attr/numeric "n1")
           (attr/numeric "n2")
           (attr/numeric "n3")]))

(def my-feature-2
  (fs/feature
   (fn [item]
     (fs/sparse-fragment {1 "a"} 3))
   :attrs [(attr/numeric "s1")
           (attr/nominal "nom1" ["a" "b"])
           (attr/numeric "s3")]))

(def my-fs
  (fs/feature-set [my-feature my-feature-2]))

(def my-data
  ["data1" "data2" "data1"])

(def binary-cls (export/class-data ["data1" "data2"] identity))

(def ranking-cls (export/class-data (fn [dp] (Math/random))))

(def my-str-writer (java.io.StringWriter.))

(def my-str-writer2 (java.io.StringWriter.))

(def my-str-writer3 (java.io.StringWriter.))

(binding [export/*arff-mode* :sparse]
  (export/weka-arff my-data my-fs binary-cls my-str-writer2))
(println (.toString my-str-writer2))

(export/weka-arff my-data my-fs binary-cls my-str-writer)
(println (.toString my-str-writer))

(export/svm-light my-data my-fs ranking-cls my-str-writer3)
(println (.toString my-str-writer3))

;;
;; Example with features derived from CSV file data
;; Where first N data points are numeric features, last one is class label
;;

(def my-csv-feature
  (fs/feature
   (fn [item]
     (map #(Integer/parseInt %) (drop-last item)) ;all but class item
     )))

(def my-csv-feature-set
  (fs/feature-set [my-csv-feature]))

(def csv-cls (export/class-data ["A" "B"] #(last %)))

(def my-csv-data
  (binding [export/*parallel* true
            fs/*parallel* true]
    (with-open [rdr (io/reader (io/resource "1million-test.csv"))
                wrt (io/writer "test.out")]
      (let [data-points (csv/parse-csv rdr)
            out (java.io.StringWriter.)]
        (time (export/svm-light data-points my-csv-feature-set csv-cls wrt))
                                        ;(println (.toString out))
        ))))
