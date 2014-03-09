;; 4Clojure Problem 147. Pascal's Trapezoid
;; url: http://www.4clojure.com/problem/147
(fn __ [coll]
  (cons coll 
        (lazy-seq 
          (__ (-> (map +' coll (rest coll))
                  (conj (first coll))
                  (vec)
                  (conj (last coll)))))))