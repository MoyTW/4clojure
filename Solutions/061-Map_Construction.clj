;; 4Clojure Problem 61. Map Construction
;; url: http://www.4clojure.com/problem/61
(fn cust-zipmap [k v]
  (apply assoc {}(interleave k v)))