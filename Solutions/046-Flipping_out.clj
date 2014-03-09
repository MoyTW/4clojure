;; 4Clojure Problem 46. Flipping out
;; url: http://www.4clojure.com/problem/46
(fn flip [f]
  (fn ([x y] (f y x))))