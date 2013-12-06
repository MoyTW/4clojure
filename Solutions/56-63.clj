;;;; -----=====***** 56 (7 lines, 7 total) *****=====-----
(fn cust-distinct [coll]
  (reduce
    (fn [m n]
	    (if (some #{n} m) m
		    (conj m n)))
	  []
  	coll))

;;;; -----=====***** 57 (1 line, 8 total) *****=====-----		
[5 4 3 2 1]

;;;; -----=====***** 58 (12 lines, 20 total) *****=====-----		
; Note that this only handles the arg cases it has to to pass the unit tests
; I think there's a "a b c" case I missed.
(fn run-in-order
  ([x] x)
  ([x y]
   (fn 
     ([arg] (x (y arg)))
     ([a b] (x (y a b)))
     ([a b c & args] (x (apply y a b c args)))))
  ([x y z]
   (fn 
     ([arg] (x (y (z arg))))
     ([a b] (x (y (z a b))))
     ([a b c & args] (x (y (apply z a b c args)))))))

;;;; -----=====***** 59 (4 lines, 24 total) *****=====-----		
(fn cust-jux
  ([x] (fn [& args] (list (apply x args))))
  ([x y] (fn [& args] (list (apply x args) (apply y args))))
  ([x y z] (fn [& args] (list (apply x args) (apply y args) (apply z args)))))
	
;;;; -----=====***** 60 (7 lines, 31 total) *****=====-----		
(fn cust-reductions
  ([f coll] (cust-reductions f (first coll) (next coll)))
  ([f init coll]
    (if (seq coll)
      (cons init 
	      (lazy-seq (cust-reductions f (f init (first coll)) (next coll))))
	    (cons init (lazy-seq '())))))
			
;;;; -----=====***** 61 (2 lines, 33 total) *****=====-----		
(fn cust-zipmap [k v]
  (apply assoc {}(interleave k v)))

;;;; -----=====***** 62 (2 lines, 35 total) *****=====-----			
(fn cust-iter [f x]
  (cons x (lazy-seq (cust-iter f (f x)))))

;;;; -----=====***** 63 (8 lines, 43 total) *****=====-----				
(fn cust-group [f s]
  (loop [f f s s m {}]
    (if (empty? s) m
	    (let [f-s (first s)]
	      (recur 
	        f 
		      (rest s)
		      (assoc m (f f-s) (conj (get m (f f-s) []) f-s)))))))