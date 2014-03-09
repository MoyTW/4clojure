;; 4Clojure Problem 63. Group a Sequence
;; url: http://www.4clojure.com/problem/63
(fn cust-group [f s]
  (loop [f f s s m {}]
    (if (empty? s)
	    m
	  (let [f-s (first s)]
	    (recur 
	      f 
		  (rest s)
		  (assoc m (f f-s) (conj (get m (f f-s) []) f-s)))))))