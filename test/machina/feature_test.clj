(ns machina.feature-test
  (:require [machina.feature :as fs]
            [machina.data :as data])
  (:use clojure.test))

(def my-feature
  (fs/feature
   (fn [item]
     (data/dense-fragment [1 2 3]))))

(def my-feature-2
  (fs/feature
   (fn [item]
     (data/sparse-fragment {1 "a"} 3))))

(def my-fs
  (fs/feature-set [my-feature my-feature-2]))

(def my-data
  ["item1" "item2"])

(deftest use-features
  (testing "Using features with some data"
    (is (=
         (map second (data/combined-seq [((:compute my-feature) "item")]))
         [1 2 3]))
    (is (=
         (map second (data/combined-seq [((:compute my-feature-2) "item")]))
         [0 "a" 0]))
    ))

(deftest use-feature-set
  (testing "Using feature set"
    (is (=
         (map second (data/combined-seq ((:compute my-fs) "item")))
         [1 2 3 0 "a" 0]
         ))
    ))
