;; 4Clojure Problem 70. Word Sorting
;; url: http://www.4clojure.com/problem/70
#(sort-by clojure.string/lower-case (re-seq #"[A-z]+" %))