;;;; -----=====***** 70 (1 line, 1 total) *****=====-----	
#(sort-by clojure.string/lower-case (re-seq #"[A-z]+" %))

;;;; -----=====***** 71 (1 line, 2 total) *****=====-----	
last

;;;; -----=====***** 72 (1 line, 3 total) *****=====-----	
apply +

;;;; -----=====***** 73 (23 lines, 26 total) *****=====-----	
; This is HORRIFYING
(fn check-lines [coll]
  (loop [rng (take 3 (range))]
    (let [three-in-row (fn three-in-row [coll]
                         (let [result (reduce #(if (= %1 %2) %1 nil) (first coll) coll)] 
                           (if (or (= result :o) (= result :x)) 
                             result 
                             nil)))]
      ; This handles diagonals
      (if (= '() rng)
        (let [r-v (three-in-row [(nth (nth coll 0) 0) (nth (nth coll 1) 1) (nth (nth coll 2) 2)])
              l-v (three-in-row [(nth (nth coll 0) 2) (nth (nth coll 1) 1) (nth (nth coll 2) 0)])]
          (if r-v
            r-v
            (if l-v l-v nil)))
        ; This handles horizontal/verticals
        (let [n (first rng)
              v-val (three-in-row (nth coll n))
              h-val (three-in-row (reduce #(conj %1 (nth %2 n)) [] coll))]
          (if v-val
            v-val
            (if h-val
              h-val
              (recur (rest rng)))))))))
							
;;;; -----=====***** 74 (11 lines, 37 total) *****=====-----	
(fn to-union [s]
  (let [nums (apply conj (sorted-set) (map #(Integer. %) (clojure.string/split s #",")))]
    (reduce #(str %1 "," %2)
            (clojure.set/intersection
             (apply conj 
                    (sorted-set) 
                    (rest (take 
                           (inc (Math/ceil (Math/sqrt (apply max nums)))) 
                           (map #(* % %) (range)))))
             nums))))

;;;; -----=====***** 75 (11 lines, 48 total) *****=====-----							 
(fn toitent [n]
  (let [gcd (fn [a b] 
              (loop [a a b b] 
                (if (= a b) 
                  a 
                  (if (> a b) 
                    (recur (- a b) b) 
                    (recur a (- b a))))))]
    (count (filter #(= (gcd % n) 1) 
                   (rest (take (inc n) (range)))))))