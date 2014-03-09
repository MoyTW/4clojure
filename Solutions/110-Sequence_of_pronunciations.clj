;; 4Clojure Problem 110. Sequence of pronunciations
;; url: http://www.4clojure.com/problem/110
(fn weird [coll]
  (letfn [(step [coll]
           (mapcat #(list (count %) (first %)) 
                   (partition-by (fn [x] x) coll)))]
    (rest (iterate step coll))))