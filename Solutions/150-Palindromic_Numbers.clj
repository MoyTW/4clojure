;;;; 150 - Palindromic Numbers (56 lines)
;; This one drove me a bit batty.
;;   The deal is this: It times out on 4Clojure. However, if you run the tests
;; locally, it'll finish in less than a second. Weird, huh?
;;   Since it passes all the tests - and fairly quickly - I just threw my hands
;; up and said "The solution is right and 4Clojure has some obscure sandbox
;; issue or something" - so here's my solution.
;;   You may also want to look here for my scratch paper:
; https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/150-wip.clj
; https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/150-synth.clj

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