;; 4Clojure Problem 78. Reimplement Trampoline
;; url: http://www.4clojure.com/problem/78
(fn cust-tramp [f & args]
  (let [result (apply f args)]
    (loop [f result]
      (if (fn? f)
        (recur (f))
        f))))