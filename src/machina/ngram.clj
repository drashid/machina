(ns machina.ngram)

(def ^{:dynamic true} *ngram-start* "<s>")

(def ^{:dynamic true} *ngram-end* "</s>")

(defn whitespace-tokenizer
  [text]
  (clojure.string/split text #"[\s]+"))

(defn ngrams
  ([n text tokenizer]
     (ngrams n (tokenizer text)))
  ([n tokenized]
     ;; capture to avoid issues with thread-local bindings and laziness
     (let [start *ngram-start*
           end   *ngram-end*]
       (partition n 1
         (lazy-cat
          (repeat (dec n) start)
          tokenized
          (repeat (dec n) end))))))
