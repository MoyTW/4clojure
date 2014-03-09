;; 4Clojure Problem 86. Happy numbers
;; url: http://www.4clojure.com/problem/86
(fn is-happy [n]
  (letfn [(to-chars [n] (reduce #(conj %1 (Character/getNumericValue %2)) [] (str n)))
          (happy-step [n] (reduce #(+ %1 (* %2 %2)) 0 (to-chars n)))
          (gen-happy [n] (cons n (lazy-seq (gen-happy (happy-step n)))))]
    (contains? (reduce #(conj %1 %2) #{} (take 100 (gen-happy n))) 1)))