;; 4Clojure Problem 103. Generating k-combinations
;; url: http://www.4clojure.com/problem/103
(fn k-comb [n st]
  (let [gen-next 
         (fn [p-sets n-set]
           (into #{} (for [p p-sets n n-set
                           :when (not (contains? p n))]
                       (conj p n))))
        st-of-sts (map (fn [n] #{n}) st)]
    (loop [i 1 out-set st-of-sts]
      (if (= i n) (into #{} out-set)
        (recur (inc i) (gen-next out-set st))))))