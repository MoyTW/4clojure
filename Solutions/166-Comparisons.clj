;; 4Clojure Problem 166. Comparisons
;; url: http://www.4clojure.com/problem/166
(fn __ [op item-one item-two]
  (cond
    (= (op item-one item-two) (op item-two item-one)) :eq
    (op item-one item-two) :lt
    :else :gt))