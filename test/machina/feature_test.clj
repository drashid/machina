(ns machina.feature-test
  (:require [machina.feature :as fs])
  (:use clojure.test))

(def my-feature
  (fs/feature
   (fn [item]
     (fs/dense-fragment [1 2 3]))))

(def my-feature-2
  (fs/feature
   (fn [item]
     (fs/sparse-fragment {1 "a"} 3))))

(def my-fs
  (fs/feature-set [my-feature my-feature-2]))

(def my-data
  ["item1" "item2"])

(deftest use-features
  (testing "Using features with some data"
    (is (=
         (map second (fs/combined-seq [((:compute my-feature) "item")]))
         [1 2 3]))
    (is (=
         (map second (fs/combined-seq [((:compute my-feature-2) "item")]))
         [0 "a" 0]))
    ))

(deftest use-feature-set
  (testing "Using feature set"
    (is (=
         (map second (fs/compute-item my-fs "item"))
         [1 2 3 0 "a" 0]
         ))
    ))

(deftest one-dense-test
  (testing "Two dense fragments combined together"
    (is (=
         (fs/combined-seq [(fs/dense-fragment (take 2 (range)))
                           (fs/dense-fragment (take 3 (range)) 3)])
         [[0 0] [1 1] [2 0] [3 1] [4 2]]))))


(deftest mix-test
  (testing "One dense fragment and one sparse fragment combined"
    (is (=
         (fs/combined-seq [(fs/dense-fragment (take 2 (range)))
                           (fs/sparse-fragment {1 "a"} 2)])
         [[0 0] [1 1] [2 0] [3 "a"]]))))

(deftest huge-dense
  (testing "Huge dense fragment!  Testing that we're not doing any O(n) operations or holding a ton in memory"
    (is (=
         (take 1 (drop 1000000 (fs/combined-seq [(fs/dense-fragment (range) 100000000)])))
         [[1000000 1000000]]))))

(deftest dense-fragment-length-lt-gt
  (testing "If a length is explicitly given for a dense fragment, it will truncate to that length"
    (is (=
            (fs/combined-seq [(fs/dense-fragment (range) 3)])
            [[0 0] [1 1] [2 2]])))
  (testing "If a length is explicitly given for a dense fragment, it will stop when it has no more elements, not fill 0s "
    (is (=
            (fs/combined-seq [(fs/dense-fragment [0 1 2] 10)])
            [[0 0] [1 1] [2 2]]))))
