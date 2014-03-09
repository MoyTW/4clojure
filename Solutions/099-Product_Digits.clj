;; 4Clojure Problem 99. Product Digits
;; url: http://www.4clojure.com/problem/99
(fn product-digits [x y]
  (map #(Integer/parseInt (str %)) (str (* x y))))