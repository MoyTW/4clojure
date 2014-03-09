;; 4Clojure Problem 112. Sequs Horribilis
;; url: http://www.4clojure.com/problem/112
(fn what [n coll]
  (letfn [(process-next [cnt in]
            (loop [cnt cnt out [] in in]
              (let [head (first in)]
                (cond
                  (= head nil) out
                  (coll? head) (conj out (process-next cnt head))
                  (> (+ cnt head) n) out
                  :else (recur (+ cnt head) (conj out head) (rest in))))))]
    (process-next 0 coll)))