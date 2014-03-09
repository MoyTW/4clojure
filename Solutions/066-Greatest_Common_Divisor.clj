;; 4Clojure Problem 66. Greatest Common Divisor
;; url: http://www.4clojure.com/problem/66
(fn gcd [a b]
  (loop [a a b b]
    (if (= a b)
      a
      (if (> a b)
        (recur (- a b) b)
        (recur a (- b a))))))