;; 4Clojure Problem 88. Symmetric Difference
;; url: http://www.4clojure.com/problem/88
(fn xor-set [lset rset]
  (cond
   (empty? lset) rset
   (empty? rset) lset
   :else 
   (let [un (clojure.set/intersection lset rset)]
     (apply conj (apply disj lset un) (apply disj rset un)))))