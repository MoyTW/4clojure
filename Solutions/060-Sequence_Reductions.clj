;; 4Clojure Problem 60. Sequence Reductions
;; url: http://www.4clojure.com/problem/60
(fn cust-reductions
  ([f coll] (cust-reductions f (first coll) (next coll)))
  ([f init coll]
    (if (seq coll)
      (cons init 
	    (lazy-seq 
  	      (cust-reductions 
            f
	        (f init (first coll))
	        (next coll))))
	  (cons init (lazy-seq '())))))