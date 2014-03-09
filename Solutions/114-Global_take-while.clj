;; 4Clojure Problem 114. Global take-while
;; url: http://www.4clojure.com/problem/114
(fn take-before-nth-match [n pred coll]
  (letfn [(count-to-take [n c coll]
            (cond
              (= n 0) c
              (pred (first coll)) (count-to-take (dec n) (inc c) (rest coll))
              :else (count-to-take n (inc c) (rest coll))))]
    (take (dec (count-to-take n 0 coll)) coll)))