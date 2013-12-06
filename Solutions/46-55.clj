; -----=====***** 46 (2 lines, 2 total) *****=====-----
(fn flip [f]
  (fn ([x y] (f y x))))
  
; -----=====***** 47 (1 line, 3 total) *****=====-----
4

; -----=====***** 48 (1 line, 4 total) *****=====-----
6

; -----=====***** 49 (5 lines, 9 total) *****=====-----
(fn [x coll]
  (loop [n x in-coll coll out-coll []]
    (if (= n 0)
	  (conj [out-coll] in-coll)
      (recur (dec n) (subvec in-coll 1) (conj out-coll (first in-coll))))))

; -----=====***** 50 (5 lines, 14 total) *****=====-----      
(fn split [coll]
  (vals (reduce
         (fn [m n] (assoc m (type n) (conj (get m (type n) []) n)))
         {}
         coll)))

; -----=====***** 51 (1 line, 15 total) *****=====-----
[1 2 3 4 5]

; -----=====***** 52 (1 line, 16 total) *****=====-----
[c e]

; -----=====***** 53 (13 lines, 29 total) *****=====-----
(fn longest [coll]
  (loop [coll coll longest (seq '())]
    (if (= (first coll) nil)
	  (if (> (count longest) 1) longest [])
	  (let [next-inc-seq 
              (loop [coll coll inc-seq [-1]]
                (if (or (= (first coll) nil) (<= (first coll) (last inc-seq)))
                  (rest inc-seq)
	              (recur (rest coll) (conj inc-seq (first coll)))))]
	    (recur (subvec coll (count next-inc-seq))
		       (if (>= (count longest) (count next-inc-seq))
			     longest
				 next-inc-seq))))))

; -----=====***** 54 (8 lines, 37 total) *****=====-----                 
(fn cust-part [x coll]
  (loop [n x seq-list (list (take x coll)) coll (drop x coll)]
    (if (< (count coll) n)
      seq-list
	  (recur
  	    n
	    (concat seq-list (list (take n coll)))
	    (drop n coll)))))

; -----=====***** 55 (5 lines, 42 total) *****=====-----                         
(fn cust-freq [coll]
  (reduce
    (fn [m n] (assoc m n (inc (get m n 0))))
	{}
	coll))