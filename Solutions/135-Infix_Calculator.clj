;; 4Clojure Problem 135. Infix Calculator
;; url: http://www.4clojure.com/problem/135
(fn __ [initial & args]
  (let [ops (partition 2 args)]
    (reduce #((first %2) %1 (second %2)) initial ops)))