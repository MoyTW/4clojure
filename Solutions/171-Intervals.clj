;; 4Clojure Problem 171. Intervals
;; url: http://www.4clojure.com/problem/171
(fn __ [coll]
  (if (seq coll)
    (let [sorted (distinct (sort coll))
          red (fn [[out head prev] in]
                (cond
                  (= (inc prev) in) [out head in]
                  :else [(conj out [head prev]) in in]))
          [out last-first last-last] 
            (reduce red [[] (first sorted) (first sorted)] (rest sorted))]
      (conj out [last-first last-last]))
    coll))