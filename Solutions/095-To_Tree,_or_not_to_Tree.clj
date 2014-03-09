;; 4Clojure Problem 95. To Tree, or not to Tree
;; url: http://www.4clojure.com/problem/95
(fn test-btree [[val l-chld r-chld :as node]]
  (cond
    (= node nil) true
    (or (not (or (= nil l-chld) (coll? l-chld)))
        (not (or (= nil r-chld) (coll? r-chld)))
        (= val nil) 
        (not= (count node) 3)) 
      false
    :else (and (test-btree l-chld) (test-btree r-chld))))