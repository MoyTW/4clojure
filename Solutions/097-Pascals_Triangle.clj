;; 4Clojure Problem 97. Pascal's Triangle
;; url: http://www.4clojure.com/problem/97
(fn nth-row [n]
  (letfn [(next-row [row]
            (let [n-row (reduce (fn [[out l] n] 
                                  [(conj out (+ l n)) n]) 
                                 [[] 0] 
                                 row)]
               (conj (first n-row) (last n-row))))]
    (nth (iterate next-row [1]) (dec n))))