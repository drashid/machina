(ns machina.data-test
  (:use clojure.test
        machina.data))

(deftest one-dense-test
  (testing "Something!"
    (is (=
         (combined-seq [(dense-fragment (take 2 (range)))
                        (dense-fragment (take 1 (range)))])
         [[0 0] [1 1] [2 0]]))))
