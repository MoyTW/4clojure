;; 4Clojure Problem 90. Cartesian Product
;; url: http://www.4clojure.com/problem/90
(fn cross-product [lset rset]
  (reduce
    (fn r [s n]
      (apply conj s (reduce #(conj %1 [%2 n]) #{} lset)))
    #{} 
    rset))