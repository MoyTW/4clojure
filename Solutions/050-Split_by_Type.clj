;; 4Clojure Problem 50. Split by Type
;; url: http://www.4clojure.com/problem/50
(fn split [coll]
  (vals (reduce
    (fn [m n]
	  (assoc m (type n) (conj (get m (type n) []) n))
	)
	{}
	coll)))