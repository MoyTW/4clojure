;; 4Clojure Problem 21. Nth Element
;; url: http://www.4clojure.com/problem/21
(fn cust-nth [coll n]
  (loop [coll coll n n]
    (if (= n 0)
      (first coll)
      (recur (rest coll) (dec n)))))