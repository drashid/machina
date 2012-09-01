(ns machina.attribute-test
  (:require [machina.attribute :as attr])
  (:use clojure.test))

(deftest validation-numeric
  (testing "Testing validations for numeric"
    (is (attr/validate (attr/numeric "numeric") 5))
    (is (not (attr/validate (attr/numeric "numeric") "not a number")))
    ))

(deftest validation-set
  (testing "Testing validations for set attribute"
    (is (attr/validate (attr/nominal "set" ["A" "B"]) "A"))
    (is (not (attr/validate (attr/nominal "set" ["A" "B"]) "C")))
    (is (not (attr/validate (attr/nominal "set" ["A" "B"]) 1)))
    ))

(deftest validation-string
  (testing "Testing validations for string attribute"
    (is (attr/validate (attr/string "string") "string"))
    (is (not (attr/validate (attr/string "string") 1)))
    ))

(deftest merge-attributes
  (testing "Merging of attributes, success and failure due to naming collision"
    (is (map :name (attr/merge-attributes [[(attr/numeric "1") (attr/numeric "2")] [(attr/numeric "3") (attr/numeric "4")]] ))
        ["1" "2" "3" "4"])
    (is (thrown? AssertionError
                 (attr/merge-attributes [[(attr/numeric "1") (attr/numeric "1")] [(attr/numeric "1") (attr/numeric "1")]])))
    ))
