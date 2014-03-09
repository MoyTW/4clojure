;; 4Clojure Problem 96. Beauty is Symmetry
;; url: http://www.4clojure.com/problem/96
(fn check-sym [[val l-tree r-tree]]
  (letfn [(reverse-btree [[val l-chld r-chld :as node]]
           (if (= node nil) nil
             [val (reverse-btree r-chld) (reverse-btree l-chld)]))]
    (= l-tree (reverse-btree r-tree))))