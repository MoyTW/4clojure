;; 4Clojure Problem 150. Palindromic Numbers
;; url: http://www.4clojure.com/problem/150
(fn __ [n]
  (letfn [(palindromic? [n]
            (let [s (.toString n)
                  take-half (fn [coll] (take (int (/ (count coll) 2)) coll))]
              (= (take-half s) (take-half (reverse s)))))
          (log-10 [n]
            (loop [out -1 in n]
              (if (zero? in)
                  out
                  (recur (inc out) (quot in 10)))))
          (pow-10 [n]
            (loop [out 1 e n]
              (if (zero? e)
                  out
                  (recur (* 10 out) (dec e)))))
          (take-half [n]
            (let [l-10 (log-10 n)
                  f (if (even? l-10) int #(inc (int %)))]
              (bigint (/ n (pow-10 (f (/ l-10 2)))))))
          (reverse-integer [n]
            (loop [out 0 in n]
              (if (zero? in)
                  out
                  (recur (+' (*' 10 out) (mod in 10)) (quot in 10)))))
          (mirror [n parity]
            (if (odd? parity)
                (+ (* n (pow-10 (log-10 n))) (reverse-integer (quot n 10)))
                (+ (* n (pow-10 (inc (log-10 n)))) (reverse-integer n))))
          (next-palindromic [n]
            (let [l-10 (log-10 n)
                  parity (dec l-10)
                  left-half (take-half n)]
              (cond
                (not (palindromic? n))
                  (let [right-half (take-half (reverse-integer n))]
                    (if (> right-half left-half)
                        (mirror (inc left-half) parity)
                        (mirror left-half parity)))
                (not= l-10 (log-10 (inc n)))
                  (if (even? parity)
                      (mirror (inc left-half) 1)
                      (mirror (quot (inc left-half) 10) 0))
                :else
                  (mirror (inc left-half) parity))))
          (make-seq [n]
            (lazy-seq (cons n (make-seq (next-palindromic n)))))]
    (if (palindromic? n)
        (make-seq n)
        (make-seq (next-palindromic n)))))