;; 4Clojure Problem 22. Count a Sequence
;; url: http://www.4clojure.com/problem/22
#( 
  (fn rec [l i] 
    (if 
      (= (first l) nil)
      i
      (rec (rest l) (+ i 1))
      )
    )
  % 0
  )