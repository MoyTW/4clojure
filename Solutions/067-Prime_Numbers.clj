;; 4Clojure Problem 67. Prime Numbers
;; url: http://www.4clojure.com/problem/67
(fn first-n-primes [n]
  (let [top-num (Math/ceil (+ (* n (Math/log n)) (* n (Math/log (Math/log n)))))]
    (loop [coll (rest (rest (take (+ 5 top-num) (range))))
           primes []]
      (if (= (count primes) n)
          primes
        (let [no-div (filter #(> (mod % (first coll)) 0) coll)]
          (recur 
            no-div
            (conj primes (first coll))))))))