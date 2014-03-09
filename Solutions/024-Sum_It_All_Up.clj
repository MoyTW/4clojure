;; 4Clojure Problem 24. Sum It All Up
;; url: http://www.4clojure.com/problem/24
#(
  (fn sum [sq, sm]
    (if 
      (= nil (first sq))
         sm
      (sum
       (rest sq) (+ sm (first sq))
      )
    )
  )
  % 0
)