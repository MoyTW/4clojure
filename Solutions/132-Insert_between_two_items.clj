;; 4Clojure Problem 132. Insert between two items
;; url: http://www.4clojure.com/problem/132
; Hey what happens if you run (__ = :eq [1 2 2 1]) do you think?
(fn __ [pred val coll]
  (mapcat #(if (pred %1 %2) [%1 val] [%1])
          coll
          (concat (rest coll) [(first coll)])))