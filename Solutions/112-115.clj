;;;; -----=====***** 112 (10 lines, 10 total) *****=====-----
(fn what [n coll]
  (letfn [(process-next [cnt in]
            (loop [cnt cnt out [] in in]
              (let [head (first in)]
                (cond
                  (= head nil) out
                  (coll? head) (conj out (process-next cnt head))
                  (> (+ cnt head) n) out
                  :else (recur (+ cnt head) (conj out head) (rest in))))))]
    (process-next 0 coll)))

;;;; -----=====***** 113 (9 lines, 19 total) *****=====-----
(fn [& args]
  (reify
    java.lang.Object
    (toString [_]
      (clojure.string/join ", " (sort args)))
    clojure.lang.ISeq
      (seq [_]
        (if (empty? args) nil
          (distinct args)))))

;;;; -----=====***** 114 (7 lines, 26 total) *****=====-----
(fn take-before-nth-match [n pred coll]
  (letfn [(count-to-take [n c coll]
            (cond
              (= n 0) c
              (pred (first coll)) (count-to-take (dec n) (inc c) (rest coll))
              :else (count-to-take n (inc c) (rest coll))))]
    (take (dec (count-to-take n 0 coll)) coll)))

;;;; -----=====***** 115 (6 lines, 32 total) *****=====-----
(fn compare-side-digits [x]
  (let [s (map #(Integer. (str %)) (str x))
        l-c (int (/ (count s) 2)) 
        r-c (Math/ceil (/ (count s) 2))]
    (= (apply + (take l-c s)) 
       (apply + (drop r-c s)))))

;;;; -----=====***** 116 (11 lines, 43 total) *****=====-----
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