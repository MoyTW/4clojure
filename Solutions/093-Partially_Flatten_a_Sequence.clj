;; 4Clojure Problem 93. Partially Flatten a Sequence
;; url: http://www.4clojure.com/problem/93
(fn c-flatten [coll] 
  (let [l (first coll) r (next coll)]
    (concat 
     (if (sequential? (first l)) (c-flatten l) [l])
     (if (sequential? (first r)) (c-flatten r)))))