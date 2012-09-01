(ns machina.ngram)

(def ^{:dynamic true} *ngram-start-symbol* "<s>")

(def ^{:dynamic true} *ngram-end-symbol* "</s>")

(defn whitespace-tokenizer
  [text]
  (clojure.string/split text #"[\s]+"))

(defn ngrams
  ([n text tokenizer]
     (ngrams n (tokenizer text)))
  ([n tokenized]
     (partition n 1
       (lazy-cat
        (repeat (dec n) *ngram-start-symbol*)
        tokenized
        (repeat (dec n) *ngram-end-symbol*)))))
