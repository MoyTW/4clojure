;; 4Clojure Problem 41. Drop Every Nth Item
;; url: http://www.4clojure.com/problem/41
(fn [v n]
  (loop [v v, n n, i 0, e (int (/ (count v) n))]
    (if (= i e)
        v
      (let [n-c (- (* n (inc i)) i)]
	    (if (= (count v) n-c)
		    (subvec v 0 (dec n-c))
          (recur (apply conj (subvec v 0 (dec n-c)) (subvec v n-c)) 
		         n 
			     (inc i) 
			     e))))))