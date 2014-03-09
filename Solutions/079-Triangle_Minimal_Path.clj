;; 4Clojure Problem 79. Triangle Minimal Path
;; url: http://www.4clojure.com/problem/79
(fn min-tri [coll]
  (letfn [(expand [coll]
                  (reduce (fn [v n] 
                            (if (coll? (last v)) 
                              (conj (pop v) (conj (last v) n) [n]) 
                              [[n] [n]]))
                          []
                          coll))
         (next-level [t-c b-c] 
                     (map #(+ (apply min %1) %2) t-c b-c))]
  (apply min (reduce #(next-level (expand %1) %2) coll))))