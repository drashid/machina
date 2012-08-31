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
    (is (not (attr/validate (attr/nominal "set" ["A" "B"]) 1)))
    ))

(deftest validation-string
  (testing "Testing validations for string attribute"
    (is (attr/validate (attr/string "string") "string"))
    (is (not (attr/validate (attr/string "string") 1)))
    ))
