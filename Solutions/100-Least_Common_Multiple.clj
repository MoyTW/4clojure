;; 4Clojure Problem 100. Least Common Multiple
;; url: http://www.4clojure.com/problem/100
(fn lcm-mult [& args]
  (letfn [(gcd [a b]
            (loop [a a b b]
              (let [[lrg sma] (reverse (sort [a b]))]
                (if (= lrg sma) lrg
                  (recur (- lrg sma) sma)))))
          (lcm [a b]
             (/ (* a b) (gcd a b)))]
    (reduce lcm args)))