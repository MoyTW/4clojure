;; 4Clojure Problem 25. Find the odd numbers
;; url: http://www.4clojure.com/problem/25
#(
  (fn odd [src, dest]
    (if (= (first src) nil)
      dest
      (if (= 1 (mod (first src) 2))
        (odd (rest src) (conj dest (first src)))
        (odd (rest src) dest)
       )
     )
   )
  %, []
)