;;;; 147 - Pascal's Trapezoid (7 lines)
(fn __ [coll]
  (cons coll 
        (lazy-seq 
          (__ (-> (map +' coll (rest coll))
                  (conj (first coll))
                  (vec)
                  (conj (last coll)))))))