;; 4Clojure Problem 28. Flatten a Sequence
;; url: http://www.4clojure.com/problem/28
(fn c-flatten [coll] 
  (let [l (first coll) r (next coll)]
    (concat 
     (if (sequential? l) (c-flatten l) [l])
     (if (sequential? r) (c-flatten r)))))