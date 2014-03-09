;; 4Clojure Problem 144. Oscilrate
;; url: http://www.4clojure.com/problem/144
(fn __ [v & funcs]
  (letfn [(oscilrate [v & [ffunc & rfunc]]
            (let [rv (ffunc v)]
              (lazy-seq (cons rv (apply oscilrate rv (conj (vec rfunc) ffunc))))))]
  (cons v (apply oscilrate v funcs))))