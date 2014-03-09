;; 4Clojure Problem 55. Count Occurrences
;; url: http://www.4clojure.com/problem/55
(fn cust-freq [coll]
  (reduce
    (fn [m n]
	  (assoc m n (inc (get m n 0))))
	{}
	coll))