;; 4Clojure Problem 120. Sum of square of digits
;; url: http://www.4clojure.com/problem/120
(fn ssqd [coll]
  (letfn [(smaller? [n]
            (let [digits (map (fn [x] (Integer. (str x))) (into [] (str n)))]
              (< n (reduce #(+ %1 (* %2 %2)) 0 digits))))]
    (count (filter smaller? coll))))