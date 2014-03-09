;; 4Clojure Problem 83. A Half-Truth
;; url: http://www.4clojure.com/problem/83
(fn long-xor [& args]
  (if (= (count (apply conj #{} args)) 2) true false))