;; 4Clojure Problem 44. Rotate Sequence
;; url: http://www.4clojure.com/problem/44
(fn rotate [r coll]
  (loop [r r coll (apply vector coll)]
    (if (= 0 r)
	    coll
	  (if (> r 0)
	    (recur (dec r) (conj (subvec coll 1) (first coll)))
		(recur (inc r) (conj (butlast coll) (last coll)))))))