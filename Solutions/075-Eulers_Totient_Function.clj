;; 4Clojure Problem 75. Euler's Totient Function
;; url: http://www.4clojure.com/problem/75
(fn toitent [n]
  (let [gcd (fn [a b] 
              (loop [a a b b] 
                (if (= a b) 
                  a 
                  (if (> a b) 
                    (recur (- a b) b) 
                    (recur a (- b a))))))]
    (count (filter #(= (gcd % n) 1) 
                   (rest (take (inc n) (range)))))))