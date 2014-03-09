;; 4Clojure Problem 84. Transitive Closure
;; url: http://www.4clojure.com/problem/84
(fn closure [st]
  (let [mp (reduce #(assoc %1 (first %2) (second %2)) {} st)]
    (reduce
      (fn close-over [out-set next-key]
        (loop [os out-set k (first next-key)]
          (let [v (get mp k)]
            (if (or (= k nil) (= v nil)) os
              (recur (conj os [(first next-key) v]) v)))))
      #{} mp)))