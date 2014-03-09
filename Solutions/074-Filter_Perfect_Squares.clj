;; 4Clojure Problem 74. Filter Perfect Squares
;; url: http://www.4clojure.com/problem/74
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