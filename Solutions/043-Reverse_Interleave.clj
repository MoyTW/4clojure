;; 4Clojure Problem 43. Reverse Interleave
;; url: http://www.4clojure.com/problem/43
(fn [coll n]
  (reduce 
    (fn [sub-seqs, next-v]
      (conj (subvec sub-seqs 1) (conj (first sub-seqs) next-v))) 
	(nth (iterate #(conj % []) []) n) 
	coll))