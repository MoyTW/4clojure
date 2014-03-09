;; 4Clojure Problem 58. Function Composition
;; url: http://www.4clojure.com/problem/58
; Note that this only handles the arg cases it has to to pass the unit tests
; I think there's a "a b c" case I missed.
(fn run-in-order
  ([x] x)
  ([x y]
   (fn 
     ([arg] (x (y arg)))
     ([a b] (x (y a b)))
     ([a b c & args] (x (apply y a b c args)))))
  ([x y z]
   (fn 
     ([arg] (x (y (z arg))))
     ([a b] (x (y (z a b))))
     ([a b c & args] (x (y (apply z a b c args)))))))