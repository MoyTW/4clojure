;; 4Clojure Problem 23. Reverse a Sequence
;; url: http://www.4clojure.com/problem/23
#(
  (fn rev [src, dest]
    (if 
      (= (first src) nil)
      dest
      (rev 
       (rest src) 
       (conj dest (first src))
      )
     )
    )
  % '()
)