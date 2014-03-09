;; 4Clojure Problem 143. dot product
;; url: http://www.4clojure.com/problem/143
(fn __ [c1 c2]
  (reduce + (map * c1 c2)))