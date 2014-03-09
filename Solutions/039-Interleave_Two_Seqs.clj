;; 4Clojure Problem 39. Interleave Two Seqs
;; url: http://www.4clojure.com/problem/39
(fn customer-interleave [left right]
  (loop [left left right right new []]
    (if (or (= (first left) nil) (= (first right) nil) )
        new
      (recur (rest left)
             (rest right)
             (conj new (first left) (first right))))))