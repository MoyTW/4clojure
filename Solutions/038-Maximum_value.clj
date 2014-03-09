;; 4Clojure Problem 38. Maximum value
;; url: http://www.4clojure.com/problem/38
(fn custom-max [& args]
  (loop [current (first args) inputs (rest args)]
    (if (= (first inputs) nil)
        current
      (recur (if (> (first inputs) current) (first inputs) current )
             (rest inputs)))))