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
  ["data1" "data2" "data3"])

(def cls (export/class-data ["data1" "data2" "data3"] identity))

(def my-str-writer (java.io.StringWriter.))

(def wrapped-writer (export/sync-java-writer my-str-writer))

(export/weka-arff my-data my-fs cls wrapped-writer)
(println (.toString my-str-writer))
