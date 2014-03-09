;; 4Clojure Problem 81. Set Intersection
;; url: http://www.4clojure.com/problem/81
(fn cust-inter [lset rset]
  (reduce #(if (contains? rset %2) (conj %1 %2) %1) 
          #{}
          lset))