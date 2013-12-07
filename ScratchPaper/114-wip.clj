;; So, what we want to do here, is...count down?

(
(fn take-before-nth-match [n pred coll]
  (lazy-seq
    (cond
      (= n 1) coll
      (pred (first coll)) (take-before-nth-match (dec n) pred (rest coll))
      :else (take-before-nth-match n pred (rest coll)))))
4 #(= 2 (mod % 3)) [2 3 5 7 11 13 17 19 23])

;; Get the count
(
(fn take-before-nth-match [n cnt pred coll]
    (cond
      (= n 0) cnt
      (pred (first coll)) (take-before-nth-match (dec n) (inc cnt) pred (rest coll))
      :else (take-before-nth-match n (inc cnt) pred (rest coll))))
4 0 #(= 2 (mod % 3)) [2 3 5 7 11 13 17 19 23])

;; Take the count
(
(fn take-before-nth-match [n pred coll]
  (letfn [(count-to-take [n c coll]
            (cond
              (= n 0) c
              (pred (first coll)) (count-to-take (dec n) (inc c) (rest coll))
              :else (count-to-take n (inc c) (rest coll))))]
    (take (dec (count-to-take n 0 coll)) coll)))
4 #(= 2 (mod % 3)) [2 3 5 7 11 13 17 19 23])