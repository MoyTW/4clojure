;; 4Clojure Problem 116. Prime Sandwich
;; url: http://www.4clojure.com/problem/116
(fn is-balanced-prime [n]
  (let [previousProbablePrime 
          (fn [n]
            (loop [n (dec n)]
              (if (.isProbablePrime (biginteger n) 500)
                n
                (recur (dec n)))))
        n-prime (.nextProbablePrime (biginteger n))
        p-prime (previousProbablePrime n)]
    (and (.isProbablePrime (biginteger n) 500) 
         (= n (/ (+ n-prime p-prime) 2)))))