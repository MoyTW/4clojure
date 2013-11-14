;;;; -----=====***** 64 (1 line, 1 total) *****=====-----	
+

;;;; -----=====***** 65 (10 lines, 11 total) *****=====-----	
(fn check-type [coll]
  (let [coll-m (conj coll {:w :x :y :z})]
	(if (= (+ 2 (count coll)) (count coll-m))
	    :map
      (let [coll-c (conj coll :z :z :x)]
        (if (= (+ 2 (count coll)) (count coll-c))
	        :set
	        (if (= (last coll-c) :x)
	          :vector
	          :list))))))
						
;;;; -----=====***** 66 (5 lines, 16 total) *****=====-----	
(fn gcd [a b]
  (loop [a a b b]
    (if (= a b) a
      (if (> a b) (recur (- a b) b)
        (recur a (- b a))))))
				
;;;; -----=====***** 67 (10 lines, 26 total) *****=====-----	
(fn first-n-primes [n]
  (let [top-num (Math/ceil (+ (* n (Math/log n)) 
	                         (* n (Math/log (Math/log n)))))]
    (loop [coll (rest (rest (take (+ 5 top-num) (range))))
           primes []]
      (if (= (count primes) n)
          primes
          (let [no-div (filter #(> (mod % (first coll)) 0) coll)]
            (recur no-div
                   (conj primes (first coll))))))))

;;;; -----=====***** 68 (1 line, 27 total) *****=====-----										 
[7, 6, 5, 4, 3]

;;;; -----=====***** 69 (14 lines, 41 total) *****=====-----
(fn cust-merge-with [f m & args]
  (reduce
    (fn merge-map [m t]
      (let [pairs (seq t)]
        (reduce
          (fn m-single [m t]
            (let [v-in-m (get m (first t))]
              (if (= nil v-in-m)
                (conj m t)
                (conj m {(first t) (f v-in-m (second t))}))))
          m
          t)))
    m
    args))