;; Okay. So:
(fn __ [n]
  (let [check-parity
          (fn [n]
            (if (zero? n)
                :o
                (if (even? (int (Math/log10 n))) :o :e)))
        take-half
          (fn take-half [n]
            (let [f (if (even? (int (Math/log10 n)))
                        #(Math/floor %)
                        #(Math/ceil %))]
              (bigint (/ n (Math/pow 10 (bigint (f (/ (Math/log10 n) 2))))))))
        reverse-integer
          (fn reverse-integer [n]
            (loop [out 0 in n]
              (if (zero? in)
                  out
                  (recur (+' (*' 10 out) (mod in 10)) (bigint (/ in 10))))))
        palindromic?
          (fn palindromic? [n]
            (let [s (.toString n)
                  take-half (fn [coll] (take (int (/ (count coll) 2)) coll))]
              (= (take-half s) (take-half (reverse s)))))
        mirror
          (fn mirror [n parity]
            (let [begin (if (= parity :o) (bigint (/ n 10)) (bigint n))]
              (loop [out n in begin]
                (if (zero? in)
                    out
                    (recur (+' (*' 10 out) (mod in 10)) (bigint (/ in 10)))))))
        next-palindromic
          (fn next-palindromic [n]
            (if (zero? n)
                1
                (let [parity (check-parity n)
                      all-nines? (not= (int (Math/log10 n)) (int (Math/log10 (inc n))))
                      next-half (inc (take-half n))]
                    (if all-nines?
                        (if (= parity :e)
                            (mirror next-half :o)
                            (mirror (int (/ next-half 10)) :e))
                        (mirror next-half parity)))))
        closest-palindromic
          (fn closest-palindromic [n]
            (if (zero? n)
                1
                (let [parity (check-parity n)
                      left-half (take-half n)
                      right-half (take-half (reverse-integer n))]
                  (if (> right-half left-half)
                      (mirror (inc left-half) parity)
                      (mirror left-half parity)))))]
    (if (palindromic? n)
        (lazy-seq (cons n (__ (next-palindromic n))))
        (let [p (closest-palindromic n)]
          (lazy-seq (cons p (__ (next-palindromic p))))))))

(defn log-10 [n]
  (loop [out -1 in n]
    (if (zero? in)
        out
        (recur (inc out) (quot in 10)))))

(defn pow-10 [n]
  (loop [out 1 e n]
    (if (zero? e)
        out
        (recur (* 10 out) (dec e)))))

(defn take-half [n]
  (let [l-10 (log-10 n)
        f (if (even? l-10) int #(inc (int %)))]
    (bigint (/ n (pow-10 (bigint (f (/ l-10 2))))))))
(take-half 0)
(take-half 123)
(take-half 1234)

(defn reverse-integer [n]
  (loop [out 0 in n]
    (if (zero? in)
        out
        (recur (+' (*' 10 out) (mod in 10)) (quot in 10)))))
(defn mirror [n parity]
  (if (odd? parity) ; :odd
      (+ (* n (pow-10 (log-10 n))) (reverse-integer (quot n 10)))
      (+ (* n (pow-10 (inc (log-10 n)))) (reverse-integer n))))

(defn palindromic? [n]
            (let [s (.toString n)
                  take-half (fn [coll] (take (int (/ (count coll) 2)) coll))]
              (= (take-half s) (take-half (reverse s)))))

(defn next-palindromic [n]
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

(defn __ [n]
  (letfn [(make-seq [n]
            (lazy-seq (cons n (make-seq (next-palindromic n)))))]
    (if (palindromic? n)
        (make-seq n)
        (make-seq (next-palindromic n)))))

(= (take 26 (__ 0))
   [0 1 2 3 4 5 6 7 8 9
    11 22 33 44 55 66 77 88 99
    101 111 121 131 141 151 161])
(= (take 16 (__ 162))
   [171 181 191 202
    212 222 232 242
    252 262 272 282
    292 303 313 323])
(= (take 6 (__ 1234550000))
   [1234554321 1234664321 1234774321
    1234884321 1234994321 1235005321])
(= (first (__ (* 111111111 111111111)))
   (* 111111111 111111111))
(= (set (take 199 (__ 0)))
   (set (map #(first (__ %)) (range 0 10000))))
(= true
   (apply < (take 6666 (__ 9999999))))
(= (nth (__ 0) 10101)
   9102019)

(time (= (nth (__ 0) 10101) 9102019)) ; 158
(time (= true (apply < (take 6666 (__ 9999999))))) ; 112
(time (= (set (take 199 (__ 0))) (set (map #(first (__ %)) (range 0 10000))))) ; 336
(time (= (first (__ (* 111111111 111111111))) (* 111111111 111111111))) ; .114
(time (take 6 (__ 1234550000))) ; .092
(time (take 16 (__ 162))) ; .084
(time (take 26 (__ 0))) ; .064