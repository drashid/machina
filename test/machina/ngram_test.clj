(ns machina.ngram-test
  (:require [machina.ngram :as ngram])
  (:use clojure.test)
  )

(deftest basic-ngrams
  (testing "Basic ngrams"
    (is (= (ngram/ngrams 1 "Test sentence" ngram/whitespace-tokenizer)
           [["Test"] ["sentence"]]
           ))
    (is (= (ngram/ngrams 2 "Test sentence" ngram/whitespace-tokenizer)
           [["<s>" "Test"] ["Test" "sentence"] ["sentence" "</s>"]]
           ))

    (is (= (first (ngram/ngrams 3 "Test sentence" ngram/whitespace-tokenizer))
           ["<s>" "<s>" "Test"]
           ))

    (is (= (last (ngram/ngrams 3 "Test sentence" ngram/whitespace-tokenizer))
           ["sentence" "</s>" "</s>"]
           ))
    ))

(deftest rebind-start-symbol-test
  (testing "Rebinding of start/end symbols"
    (is (=
         (binding [ngram/*ngram-start* "START"
                   ngram/*ngram-end* "END"]
           (ngram/ngrams 2 "Test sentence" ngram/whitespace-tokenizer))
         [["START" "Test"] ["Test" "sentence"] ["sentence" "END"]]))
    ))
