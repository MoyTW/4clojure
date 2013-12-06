;;;; -----=====***** 83 (2 lines, 2 total) *****=====-----
(fn long-xor [& args]
  (if (= (count (apply conj #{} args)) 2) true false))

;;;; -----=====***** 84 (9 lines, 11 total) *****=====-----
(fn closure [st]
  (let [mp (reduce #(assoc %1 (first %2) (second %2)) {} st)]
    (reduce
      (fn close-over [out-set next-key]
        (loop [os out-set k (first next-key)]
          (let [v (get mp k)]
            (if (or (= k nil) (= v nil)) os
              (recur (conj os [(first next-key) v]) v)))))
      #{} mp)))

;;;; -----=====***** 85 (8 lines, 19 total) *****=====-----
(fn power-set [trgt-st]
  (letfn [(break-up [st]
                    (reduce #(conj %1 (disj st %2)) #{} st))
          (next-line [st]
                     (reduce #(apply conj %1 (break-up %2)) #{} st))]
    (loop [st #{trgt-st} pwr-st #{trgt-st}]
      (if (= st #{#{}}) pwr-st
        (recur (next-line st) (apply conj pwr-st (next-line st)))))))

;;;; -----=====***** 86 (5 lines, 24 total) *****=====-----
(fn is-happy [n]
  (letfn [(to-chars [n] (reduce #(conj %1 (Character/getNumericValue %2)) [] (str n)))
          (happy-step [n] (reduce #(+ %1 (* %2 %2)) 0 (to-chars n)))
          (gen-happy [n] (cons n (lazy-seq (gen-happy (happy-step n)))))]
    (contains? (reduce #(conj %1 %2) #{} (take 100 (gen-happy n))) 1)))

;;;; -----=====***** 87 (0 lines, 24 total) *****=====-----
;;;; NO SUCH PROBLEM EXISTS
	
;;;; -----=====***** 88 (7 lines, 31 total) *****=====-----	
(fn xor-set [lset rset]
  (cond
   (empty? lset) rset
   (empty? rset) lset
   :else 
   (let [un (clojure.set/intersection lset rset)]
     (apply conj (apply disj lset un) (apply disj rset un)))))

;;;; -----=====***** 89 (20 lines, 51 total) *****=====-----	 
(fn assumes-connected-euler? [edge-vec]
  (letfn [(record-node [m n]
                       (let [f-n (first n) 
                             l-n (last n)
                             with-f (assoc m f-n (inc (get m f-n 0)))]
                         (assoc with-f l-n (inc (get with-f l-n 0)))))
          (fold [m n]
                (let [f-n (first n) l-n (last n)]
                  (if (not= f-n l-n) (conj m f-n l-n) m)))
          (to-set [edges]
                  (apply conj #{} (flatten edges)))
          (connected? [edges]
                      (if (= (to-set edges) (reduce fold #{} edges))
                        true
                        false))]
    (if-not (connected? edge-vec) false
      (->> edge-vec
           (reduce record-node {})
           (filter #(odd? (last %)))
           (count)
           (>= 2)))))