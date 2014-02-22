;; That's an interesting function that it wants.

(defn __ [v & funcs]
  (let [rv ((first funcs) v)]
    (lazy-seq (cons rv (apply __ rv (conj (vec (rest funcs)) (first funcs)))))))
(take 3 (__ 3.14 int double))
;; oh it wants first as first

(defn __ [v & [ffunc & rfunc]]
  (let [rv (ffunc v)]
    (lazy-seq (cons rv (apply __ rv (conj (vec rfunc) ffunc))))))
(take 3 (__ 3.14 int double))

(fn __ [v & funcs]
  (letfn [(oscilrate [v & [ffunc & rfunc]]
            (let [rv (ffunc v)]
              (lazy-seq (cons rv (apply oscilrate rv (conj (vec rfunc) ffunc))))))]
  (cons v (apply oscilrate v funcs))))
(take 3 (__ 3.14 int double))