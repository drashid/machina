(ns machina.export-test
  (:require [machina.export :as export]
            [machina.attribute :as attr]
            [machina.feature :as fs])
  (:use clojure.test))

(def my-feature
  (fs/feature
   (fn [item]
     (fs/dense-fragment [1 2 3]))
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

(def cls (export/binary-class-data ["data1" "data2"] identity))

(def my-str-writer (java.io.StringWriter.))
(def wrapped-writer (export/sync-java-writer my-str-writer))

(def my-str-writer2 (java.io.StringWriter.))
(def wrapped-writer2 (export/sync-java-writer my-str-writer2))

(def my-str-writer3 (java.io.StringWriter.))
(def wrapped-writer3 (export/sync-java-writer my-str-writer3))

(binding [export/*arff-mode* :sparse]
  (export/weka-arff my-data my-fs cls wrapped-writer2))
(println (.toString my-str-writer2))

(export/weka-arff my-data my-fs cls wrapped-writer)
(println (.toString my-str-writer))

(export/svm-light my-data my-fs cls wrapped-writer3)
(println (.toString my-str-writer3))
