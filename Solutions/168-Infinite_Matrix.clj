;; 4Clojure Problem 168. Infinite Matrix
;; url: http://www.4clojure.com/problem/168
(fn __
  ([f]
    (__ f 0 0))
  ([f m n]
    (letfn [(ascending 
              ([start]
                (lazy-seq (cons start (ascending (inc start)))))
              ([start end]
                (take (- end start) (ascending start))))
            (build-row [f r start]
              (let [a (ascending start)]
                (map #(f r %) a)))]
    (let [a (ascending m)]
      (map #(build-row f % n) a))))
  ([f m n s t]
    (let [infinite (__ f m n)]
      (take s (map #(take t %) infinite)))))