;; 4Clojure Problem 82. Word Chains
;; url: http://www.4clojure.com/problem/82
(fn oh-god-no [s]
  (letfn [(diff-by-del [long-w short-w]
            (loop [n (count long-w)]
              (cond
               (= n 0) false
               (= (str (subs long-w 0 (dec n)) (subs long-w n)) short-w) true
               :else (recur (dec n)))))
          (diff-by-sub [l-w r-w]
            (if (= 1 (reduce #(if %2 %1 (inc %1)) 
                             0 
                             (map #(= %1 %2) l-w r-w)))
              true
              false))
          (diff-by-one [l-w r-w]
            (let [l-cnt (count l-w) r-cnt (count r-w)]
              (cond 
               (= l-cnt r-cnt) (diff-by-sub l-w r-w)
               (= (inc l-cnt) r-cnt) (diff-by-del r-w l-w)
               (= (inc r-cnt) l-cnt) (diff-by-del l-w r-w)
               :else false)))
          (find-one-shifts [wrd st]
            (filter #(diff-by-one wrd %) st))
          (set-to-sorted-map [s]
            (let [unsorted (reduce #(assoc %1 %2 (find-one-shifts %2 s)) {} s)]
              (into (sorted-map-by #(compare [(count (get unsorted %1)) %1]
                                             [(count (get unsorted %2)) %2]))
                    unsorted)))]
    (let [get-lowest-count (fn [st mp] (first (sort-by #(count (get mp %1)) st)))
          mp (set-to-sorted-map s)]
      (loop [head (last (first mp)) mp (dissoc mp (ffirst mp))]
        (let [p-lnks (filter #(contains? mp %) head)]
          (cond
           (empty? mp) true
           (empty? p-lnks) false
           :else
           (let [next-key (get-lowest-count p-lnks mp)]
             (recur (get mp next-key) (dissoc mp next-key)))))))))