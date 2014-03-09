;; 4Clojure Problem 158. Decurry
;; url: http://www.4clojure.com/problem/158
(fn __ [f]
  (fn [& args]
    (loop [r f args args]
      (if (seq args)
          (recur (r (first args)) (rest args))
          r))))