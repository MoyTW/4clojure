;; 4Clojure Problem 125. Gus' Quinundrum
;; url: http://www.4clojure.com/problem/125
(fn [] (let [x ["(fn [] (let [x " "] (str (first x) x (last x))))"]] (str (first x) x (last x))))