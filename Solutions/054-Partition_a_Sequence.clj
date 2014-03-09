;; 4Clojure Problem 54. Partition a Sequence
;; url: http://www.4clojure.com/problem/54
(fn cust-part [x coll]
  (loop [n x seq-list (list (take x coll)) coll (drop x coll)]
    (if (< (count coll) n)
      seq-list
	  (recur
  	    n
	    (concat seq-list (list (take n coll)))
	    (drop n coll)))))