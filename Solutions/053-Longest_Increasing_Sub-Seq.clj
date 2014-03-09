;; 4Clojure Problem 53. Longest Increasing Sub-Seq
;; url: http://www.4clojure.com/problem/53
(fn longest [coll]
  (loop [coll coll longest (seq '())]
    (if (= (first coll) nil)
	  (if (> (count longest) 1)
	    longest
		[])
	  (let [next-inc-seq (loop [coll coll inc-seq [-1]]
                           (if (or (= (first coll) nil) (<= (first coll) (last inc-seq)))
                               (rest inc-seq)
	                       (recur (rest coll) (conj inc-seq (first coll)))))]
	    (recur (subvec coll (count next-inc-seq))
		       (if (>= (count longest) (count next-inc-seq))
			     longest
				 next-inc-seq))))))