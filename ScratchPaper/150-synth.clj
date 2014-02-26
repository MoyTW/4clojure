(fn __ [n]
  (let [palindromic?
          (fn palindromic? [n]
            (let [take-half (fn [coll] (take (int (/ (count coll) 2)) coll))]
              (= (take-half (.toString n)) (take-half (reverse (.toString n))))))
        next-palindromic
          (fn next-palindromic [n]
            (let [pivot (fn [s] (concat s (rest (reverse s))))
                  mirror (fn [s] (concat s (reverse s)))
                  is-even (even? (count (.toString n)))
                  to-int (fn [coll] (Long/parseLong (apply str coll)))
                  all-nines? (apply = \9 (.toString n))
                  next-half (->> (.toString n)
                                 (take (Math/ceil (/ (count (.toString n)) 2)))
                                 (to-int)
                                 (inc)
                                 (.toString))]
              (if is-even
                  (if all-nines?
                      (to-int (pivot next-half))
                      (to-int (mirror next-half)))
                  (if all-nines?
                      (to-int (mirror (butlast next-half)))
                      (to-int (pivot next-half))))))]
    (if (palindromic? n)
        (lazy-seq (cons n (__ (next-palindromic n))))
        (let [p (first (filter palindromic? (map  #(+ % n) (range))))]
          (lazy-seq (cons p (__ (next-palindromic p))))))))
;; Timed out!?

(time (= (nth (__ 0) 10101) 9102019)) ; 534
(time (= true (apply < (take 6666 (__ 9999999))))) ; 302
(time (= (set (take 199 (__ 0))) (set (map #(first (__ %)) (range 0 10000))))) ; 8477 - oh, this one. Ah. It's more or less specifically designed to mess with the way in which I find "next palindromic"...

(defn __ [n]
  (let [palindromic?
          (fn palindromic? [n]
            (let [take-half (fn [coll] (take (int (/ (count coll) 2)) coll))]
              (= (take-half (.toString n)) (take-half (reverse (.toString n))))))
        pivot (fn [s] (concat s (rest (reverse s))))
        mirror (fn [s] (concat s (reverse s)))
        is-even (even? (count (.toString n)))
        to-int (fn [coll] (Long/parseLong (apply str coll)))
        next-palindromic
          (fn next-palindromic [n]
            (let [all-nines? (apply = \9 (.toString n))
                  next-half (->> (.toString n)
                                 (take (Math/ceil (/ (count (.toString n)) 2)))
                                 (to-int)
                                 (inc)
                                 (.toString))]
              (if is-even
                  (if all-nines?
                      (to-int (pivot next-half))
                      (to-int (mirror next-half)))
                  (if all-nines?
                      (to-int (mirror (butlast next-half)))
                      (to-int (pivot next-half))))))
          closest-palindromic
            (fn closest-palindromic [n]
              (let [take-half (fn [coll]
                                (->> coll
                                     (take (Math/ceil (/ (count (.toString n)) 2)))
                                     (to-int)))
                    left-half (take-half (.toString n))
                    right-half (take-half (reverse (.toString n)))]
                 (if (> right-half left-half)
                    (if is-even 
                        (to-int (mirror (.toString (inc left-half))))
                        (to-int (pivot (.toString (inc left-half)))))
                    (if is-even
                        (to-int (mirror (.toString left-half)))
                        (to-int (pivot (.toString left-half)))))))]
    (if (palindromic? n)
        (lazy-seq (cons n (__ (next-palindromic n))))
        (let [p (closest-palindromic n)]
          (lazy-seq (cons p (__ (next-palindromic p))))))))

(time (= (nth (__ 0) 10101) 9102019)) ; 517
(time (= true (apply < (take 6666 (__ 9999999))))) ; 289
(time (= (set (take 199 (__ 0))) (set (map #(first (__ %)) (range 0 10000))))) ; 811
(time (= (first (__ (* 111111111 111111111))) (* 111111111 111111111))) ;.205
(time (take 6 (__ 1234550000))) ;.366
(time (take 16 (__ 162))) ;.771
(time (take 26 (__ 0))) ;.089
;; Longest is less than one second in repl, why is it timing out in 4Clojure? The longest it should be taking for any problem should be a second, but it appears to be processing for significantly longer than that. Is 4Clojure running on a potato or something?

(defn __ [n]
  (let [palindromic?
          (fn palindromic? [n]
            (let [take-half (fn [coll] (take (int (/ (count coll) 2)) coll))]
              (= (take-half (.toString n)) (take-half (reverse (.toString n))))))
        dup (fn dup [out in]
              (if (zero? in) out (recur (+ (* 10 out) (mod in 10)) (int (/ in 10)))))
        pivot (fn [s] (dup n (int (/ n 10))))
        mirror (fn [s] (dup n n))
        is-even (even? (count (.toString n)))
        to-int (fn [coll] (Long/parseLong (apply str coll)))
        next-palindromic
          (fn next-palindromic [n]
            (let [all-nines? (apply = \9 (.toString n))
                  next-half (->> (.toString n)
                                 (take (Math/ceil (/ (count (.toString n)) 2)))
                                 (to-int)
                                 (inc))]
              (if is-even
                  (if all-nines?
                      (pivot next-half)
                      (mirror next-half))
                  (if all-nines?
                      (mirror (int (/ next-half 10)))
                      (pivot next-half)))))
          closest-palindromic
            (fn closest-palindromic [n]
              (let [take-half (fn [coll]
                                (->> coll
                                     (take (Math/ceil (/ (count (.toString n)) 2)))
                                     (to-int)))
                    left-half (take-half (.toString n))
                    right-half (take-half (reverse (.toString n)))]
                 (if (> right-half left-half)
                    (if is-even 
                        (mirror (inc left-half))
                        (pivot (inc left-half)))
                    (if is-even
                        (mirror left-half)
                        (pivot left-half)))))]
    (if (palindromic? n)
        (lazy-seq (cons n (__ (next-palindromic n))))
        (let [p (closest-palindromic n)]
          (lazy-seq (cons p (__ (next-palindromic p))))))))
(time (= (nth (__ 0) 10101) 9102019)) ; 517
(time (= true (apply < (take 6666 (__ 9999999))))) ; 289
(time (= (set (take 199 (__ 0))) (set (map #(first (__ %)) (range 0 10000))))) ; 811
(time (= (first (__ (* 111111111 111111111))) (* 111111111 111111111))) ;.205
(time (take 6 (__ 1234550000))) ;.366
(time (take 16 (__ 162))) ;.771
(time (take 26 (__ 0))) ;.089

(defn __ [n]
  (let [palindromic?
          (fn palindromic? [n]
            (let [as-string (.toString n)
                  take-half (fn [coll] (take (int (/ (count coll) 2)) coll))]
              (= (take-half as-string) (take-half (reverse as-string)))))
        mirror
          (fn mirror [n parity]
            (let [begin (if (= parity :o) (bigint (/ n 10)) (bigint n))]
              (loop [out n in begin]
                (if (zero? in)
                    out
                    (recur (+' (*' 10 out) (mod in 10)) (bigint (/ in 10)))))))
        next-palindromic
          (fn next-palindromic [n]
            (let [as-str (.toString n)
                  parity (if (even? (count as-str)) :e :o)
                  all-nines? (apply = \9 as-str)
                  next-half (->> as-str
                                 (take (Math/ceil (/ (count (.toString n)) 2)))
                                 (apply str)
                                 (Long/parseLong)
                                 (inc))]
              (if all-nines?
                  (if (= parity :e)
                      (mirror next-half :o)
                      (mirror (int (/ next-half 10)) :e))
                  (mirror next-half parity))))
        closest-palindromic
          (fn closest-palindromic [n]
            (let [as-str (.toString n)
                  parity (if (even? (count as-str)) :e :o)
                  take-half (fn [coll]
                              (->> coll
                                   (take (Math/ceil (/ (count as-str) 2)))
                                   (apply str)
                                   (Long/parseLong)))
                  left-half (take-half as-str)
                  right-half (take-half (reverse as-str))]
              (if (> right-half left-half)
                  (mirror (inc left-half) parity)
                  (mirror left-half parity))))]
    (if (palindromic? n)
        (lazy-seq (cons n (__ (next-palindromic n))))
        (let [p (closest-palindromic n)]
          (lazy-seq (cons p (__ (next-palindromic p))))))))
(time (= (set (take 199 (__ 0))) (set (map #(first (__ %)) (range 0 10000))))) ; 411
(time (= (first (__ (* 111111111 111111111))) (* 111111111 111111111))) ;.25
(time (= (nth (__ 0) 10101) 9102019)) ; 227
;; aaaand it still times out. wtf?

(defn __ [n]
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
(time (= (set (take 199 (__ 0))) (set (map #(first (__ %)) (range 0 10000))))) ; 308
(time (= (first (__ (* 111111111 111111111))) (* 111111111 111111111))) ; .236
(time (= (nth (__ 0) 10101) 9102019)) ; 118
;; OH COME ON
;; What's going on here?

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
;; Well, these are all true, so I'm declaring victory, since 4Clojure appears incapable of processing this problem.