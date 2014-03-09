;; 4Clojure Problem 148. The Big Divide
;; url: http://www.4clojure.com/problem/148
(fn __ [n a b]
  (let [sum-under (fn [n] (/ (* n (inc n)) 2))
        sum-all (fn [t] (* t (sum-under (bigint (Math/floor (/ (- n 1) t))))))]
     (- (+ (sum-all a) (sum-all b)) (sum-all (* a b)))))