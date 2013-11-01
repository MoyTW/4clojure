; Easy, just disj the intersection
(fn xor-set [lset rset]
  (let [un (clojure.set/intersection lset rset)]
    (apply conj (apply disj lset un) (apply disj rset un))))

; Oh, empty sets. Right.
; Well...let's be dumb about it!
(fn xor-set [lset rset]
  (cond
   (empty? lset) rset
   (empty? rset) lset
   :else 
   (let [un (clojure.set/intersection lset rset)]
     (apply conj (apply disj lset un) (apply disj rset un)))))

; This definitely provides evidence for the importance of unit testing because
; otherwise I would have breezed through...