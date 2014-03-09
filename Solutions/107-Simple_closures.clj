;; 4Clojure Problem 107. Simple closures
;; url: http://www.4clojure.com/problem/107
(fn build-func [n]
  (fn f-of-x [x] (int (Math/pow x n))))