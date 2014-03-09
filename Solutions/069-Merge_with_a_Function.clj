;; 4Clojure Problem 69. Merge with a Function
;; url: http://www.4clojure.com/problem/69
(fn cust-merge-with [f m & args]
  (reduce
    (fn merge-map [m t]
      (let [pairs (seq t)]
        (reduce
          (fn m-single [m t]
            (let [v-in-m (get m (first t))]
              (if (= nil v-in-m)
                (conj m t)
              (conj m {(first t) (f v-in-m (second t))}))))
          m
          t)))
    m
    args))