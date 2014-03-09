;; 4Clojure Problem 121. Universal Computation Engine
;; url: http://www.4clojure.com/problem/121
(fn return-eval [l]
  (fn e-map [m]
    (letfn [(e-p [p]
              (cond
                (list? p) (e p)
                (number? p) p
                :else (m p)))
            (e [[op & args]]
              (let [e-args (map e-p args)]
                (cond
                  (= op '*) (apply * e-args)
                  (= op '+) (apply + e-args)
                  (= op '-) (apply - e-args)
                  (= op '/) (apply / e-args))))]
      (e l))))