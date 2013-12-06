;;;; -----=====***** 90 (6 lines, 6 total) *****=====-----
(fn cross-product [lset rset]
  (reduce
    (fn r [s n]
      (apply conj s (reduce #(conj %1 [%2 n]) #{} lset)))
    #{} 
    rset))
	
;;;; -----=====***** 91 (19 lines, 25 total) *****=====-----
(fn connected? [edges]
  (letfn [(group [sets edge]
                 (let [gt-cntns (fn [sets node]
                                  (reduce #(if (contains? %2 node) %2 %1) 
                                          nil 
                                          sets))
                       contains-left (gt-cntns sets (first edge))
                       contains-right (gt-cntns sets (last edge))]
                   (cond
                    (and (set? contains-left) (set? contains-right)) 
                    (conj (disj sets contains-left contains-right) 
                          (clojure.set/union contains-left contains-right))
                    (set? contains-left) (conj (disj sets contains-left) 
                                               (apply conj contains-left edge))
                    (set? contains-right) (conj (disj sets contains-right) 
                                                (apply conj contains-right edge))
                    :else
                    (conj sets (apply conj #{} edge)))))]
    (= 1 (count (reduce group #{} edges)))))
	
;;;; -----=====***** 92 (15 lines, 40 total) *****=====-----
(fn to-arabic [s]
  (let [numerals {"" 0 "I" 1, "IV" 4, "IX", 9, "V" 5, "X" 10, "XL" 40, "XC" 90,
        "L" 50, "C" 100, "CD" 400, "CM" 900, "D" 500, "M" 1000}
        p-n (fn [sum-hold n]
              (let [i-sum (first sum-hold)
                    i-hold (last sum-hold)
                    n-str (str i-hold n)]
                (condp contains? n-str
                  #{"I" "X" "C"} [i-sum n-str]
                  #{"II" "XX" "CC"} [(+ i-sum (get numerals (str i-hold))) n]
                  #{"IV" "IX" "XL" "XC" "CD" "CM"} 
                    [(+ i-sum (get numerals n-str)) nil]
                  [(+ i-sum (get numerals (str i-hold))) n])))]
    (let [pair (reduce p-n [0 nil] s)]
      (+ (first pair) (get numerals (str (last pair)))))))

;;;; -----=====***** 93 (5 lines, 45 total) *****=====-----	  
(fn c-flatten [coll] 
  (let [l (first coll) r (next coll)]
    (concat 
     (if (sequential? (first l)) (c-flatten l) [l])
     (if (sequential? (first r)) (c-flatten r)))))