;; So we could use reduce (two-step).
;; What else could we use?
;; We could use a 1-shift.
(fn __ [coll]
  (conj (vec (conj (map + coll (rest coll)) (first coll))) (last coll)))
  
(fn __ [coll]
  (conj (vec (conj (map + coll (rest coll)) 
                   (first coll))) 
        (last coll)))

(
(fn __ [coll]
  (->(map + coll (rest coll))
     (conj (first coll))
     (vec)
     (conj (last coll))))
[3 1 2])

(fn __ [coll]
  (cons coll 
        (lazy-seq 
          (__ (-> (map + coll (rest coll))
                  (conj (first coll))
                  (vec)
                  (conj (last coll)))))))

;; Integer overflow? uuuuh?
(fn __ [coll]
  (cons coll 
        (lazy-seq 
          (__ (-> (map +' coll (rest coll))
                  (conj (first coll))
                  (vec)
                  (conj (last coll)))))))