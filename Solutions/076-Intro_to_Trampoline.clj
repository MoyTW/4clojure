;; 4Clojure Problem 76. Intro to Trampoline
;; url: http://www.4clojure.com/problem/76
; Here is what is happening:
; foo is called with [] 1, returns #(bar [1] 1)
; #(bar [1] 1) returns #(foo [1] 3)
; #(foo [1] 3) returns #(bar [1 3] 3)
; #(bar [1 3] 3) returns #(foo [1 3] 5)
; and so on and so on until (> (last [x] 10))
[1 3 5 7 9 11]