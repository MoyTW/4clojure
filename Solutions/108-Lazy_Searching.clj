;; 4Clojure Problem 108. Lazy Searching
;; url: http://www.4clojure.com/problem/108
(fn lazy-search [& seqs]
  (if (= 1 (count seqs)) (ffirst seqs)
    (loop [seqs seqs]
      (let [firstvals (map first seqs)
            low-val (apply min firstvals)]
        (if (apply = firstvals) low-val
          (recur (map #(if (= low-val (first %)) (rest %) %) seqs)))))))