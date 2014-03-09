;; 4Clojure Problem 26. Fibonacci Sequence
;; url: http://www.4clojure.com/problem/26
#(
  ; Hacky! Doesn't handle fib(1) or fib(2)!
  (fn fib [n, sq]
    (if (= n 0)
      sq
      (fib 
       (- n 1) 
       (conj sq (+ 
        (nth sq (- (count sq) 2 ))
        (last sq))
       )
      )
    )
  )
  (- % 2) [1 1]
)