; -----=====***** 38 (5 lines, 5 total) *****=====-----
(fn custom-max [& args]
  (loop [current (first args) inputs (rest args)]
    (if (= (first inputs) nil) current
      (recur (if (> (first inputs) current) (first inputs) current )
             (rest inputs)))))
             
; -----=====***** 39 (7 lines, 12 total) *****=====-----
(fn customer-interleave [left right]
  (loop [left left right right new []]
    (if (or (= (first left) nil) (= (first right) nil) )
      new
      (recur (rest left)
             (rest right)
             (conj new (first left) (first right))))))
             
; -----=====***** 40 (3 lines, 15 total) *****=====-----
(fn custom-interpose [sep, in-seq]
  (butlast (flatten 
   (map (fn [in, sep] [in sep]) in-seq (take (count in-seq) (repeat sep))))))

; -----=====***** 41 (9 lines, 24 total) *****=====-----
(fn [v n]
  (loop [v v, n n, i 0, e (int (/ (count v) n))]
    (if (= i e) v
      (let [n-c (- (* n (inc i)) i)]
	    (if (= (count v) n-c) (subvec v 0 (dec n-c))
          (recur (apply conj (subvec v 0 (dec n-c)) (subvec v n-c)) 
		         n 
			     (inc i) 
			     e))))))

; -----=====***** 42 (1 line, 25 total) *****=====-----
#(reduce * (range 1 (inc %)))

; -----=====***** 43 (6 lines, 31 total) *****=====-----
(fn [coll n]
  (reduce 
    (fn [sub-seqs, next-v]
      (conj (subvec sub-seqs 1) (conj (first sub-seqs) next-v))) 
	(nth (iterate #(conj % []) []) n) 
	coll))
    
; -----=====***** 44 (7 lines, 38 total) *****=====-----
(fn rotate [r coll]
  (loop [r r coll (apply vector coll)]
    (if (= 0 r)
	  coll
	  (if (> r 0)
	    (recur (dec r) (conj (subvec coll 1) (first coll)))
		(recur (inc r) (conj (butlast coll) (last coll)))))))
        
; -----=====***** 44 (1 line, 39 total) *****=====-----
[1 4 7 10 13]

; -----=====***** 45 (2 lines, 41 total) *****=====-----
(fn flip [f]
  (fn ([x y] (f y x))))