(ns machina.data-test
  (:use clojure.test
        machina.data))

(deftest one-dense-test
  (testing "Two dense fragments combined together"
    (is (=
         (combined-seq [(dense-fragment (take 2 (range)))
                        (dense-fragment (take 3 (range)) 3)])
         [[0 0] [1 1] [2 0] [3 1] [4 2]]))))


(deftest mix-test
  (testing "One dense fragment and one sparse fragment combined"
    (is (=
         (combined-seq [(dense-fragment (take 2 (range)))
                        (sparse-fragment {1 "a"} 2)])
         [[0 0] [1 1] [2 0] [3 "a"]]))))

(deftest huge-dense
  (testing "Huge dense fragment!  Testing that we're not doing any O(n) operations or holding a ton in memory"
    (is (=
         (take 1 (drop 1000000 (combined-seq [(dense-fragment (range) 100000000)])))
         [[1000000 1000000]]))))

(deftest dense-fragment-length-lt-gt
  (testing "If a length is explicitly given for a dense fragment, it will truncate to that length"
    (is (=
            (combined-seq [(dense-fragment (range) 3)])
            [[0 0] [1 1] [2 2]])))
  (testing "If a length is explicitly given for a dense fragment, it will stop when it has no more elements, not fill 0s "
    (is (=
            (combined-seq [(dense-fragment [0 1 2] 10)])
            [[0 0] [1 1] [2 2]]))))
