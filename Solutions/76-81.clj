;;;; -----=====***** 76 (1 line, 1 total) *****=====-----	
; Here is what is happening:
; foo is called with [] 1, returns #(bar [1] 1)
; #(bar [1] 1) returns #(foo [1] 3)
; #(foo [1] 3) returns #(bar [1 3] 3)
; #(bar [1 3] 3) returns #(foo [1 3] 5)
; and so on and so on until (> (last [x] 10))
[1 3 5 7 9 11]

;;;; -----=====***** 77 (11 lines, 12 total) *****=====-----	
; Out-of-band comment: I have no idea how to format this
; It is so terrible
(fn anag [coll]
  (reduce #(conj %1 %2) #{}
          (filter #(> (count %) 1)
                  (vals ((fn to-map [coll] 
                           (reduce 
                             (fn [m n]
                               (let [k (apply conj #{} n)] 
                               (assoc m k (conj (get m k #{}) n)))) 
														 {} 
														 coll)) 
													coll)))))
													
;;;; -----=====***** 78 (6 lines, 18 total) *****=====-----	
(fn cust-tramp [f & args]
  (let [result (apply f args)]
    (loop [f result]
      (if (fn? f)
        (recur (f))
        f))))
				
;;;; -----=====***** 79 (11 lines, 29 total) *****=====-----	
(fn min-tri [coll]
  (letfn [(expand [coll]
                  (reduce (fn [v n] 
                            (if (coll? (last v)) 
                              (conj (pop v) (conj (last v) n) [n]) 
                              [[n] [n]]))
                          []
                          coll))
         (next-level [t-c b-c] 
                     (map #(+ (apply min %1) %2) t-c b-c))]
  (apply min (reduce #(next-level (expand %1) %2) coll))))

;;;; -----=====***** 80 (5 lines, 34 total) *****=====-----		
(fn [n] 
  (= n (apply 
        + 
        (filter #(= 0 (mod n %)) 
                (rest (take n (range)))))))

;;;; -----=====***** 81 (4 lines, 38 total) *****=====-----
(fn cust-inter [lset rset]
  (reduce #(if (contains? rset %2) (conj %1 %2) %1) 
          #{}
          lset))
					
;;;; Stopping here because 82 is, uh. Well. You'll see for yourself.