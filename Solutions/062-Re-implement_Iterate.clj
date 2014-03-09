;; 4Clojure Problem 62. Re-implement Iterate
;; url: http://www.4clojure.com/problem/62
(fn cust-iter [f x]
  (cons x (lazy-seq (cust-iter f (f x)))))