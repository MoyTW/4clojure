; This it?
(defn cust-inter [lset rset]
  (reduce #(if (contains? rset %2) (conj %1 %2) %1) lset #{}))
  
(= ((cust-inter #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d}))
(= (cust-inter #{0 1 2 3} #{2 3 4 5}) #{2 3}))
; Nope

; Hold on what's it doing?
(cust-inter #{0 1 2 3} #{2 3 4 5})

; whaaaat
; oh wait
(defn cust-inter [lset rset]
  (reduce #(if (contains? rset %2) (conj %1 %2) %1) #{} lset))
; My reduce syntax is not up to snuff, accidentally put the initial value in the wrong place.

; For 4Clojure:
(fn cust-inter [lset rset]
  (reduce #(if (contains? rset %2) (conj %1 %2) %1) 
          #{} 
          lset))