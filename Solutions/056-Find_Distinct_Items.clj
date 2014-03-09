;; 4Clojure Problem 56. Find Distinct Items
;; url: http://www.4clojure.com/problem/56
(fn cust-distinct [coll]
  (reduce
    (fn [m n]
	  (if (some #{n} m)
	    m
		(conj m n)))
	[]
	coll))