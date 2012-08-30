(ns machina.data-test
  (:use clojure.test
        machina.data))

(deftest one-dense-test
  (testing "Two dense vectors combined"
    (is (=
         (combined-seq [(dense-fragment (take 2 (range)))
                        (dense-fragment (take 3 (range)) 3)])
         [[0 0] [1 1] [2 0] [3 1] [4 2]]))))


(deftest mix-test
  (testing "One dense vector and one sparse vector combined"
    (is (=
         (combined-seq [(dense-fragment (take 2 (range)))
                        (sparse-fragment {1 "a"} 2)])
         [[0 0] [1 1] [2 0] [3 "a"]]))))
