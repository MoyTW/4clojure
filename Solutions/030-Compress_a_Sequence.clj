;; 4Clojure Problem 30. Compress a Sequence
;; url: http://www.4clojure.com/problem/30
#(
  (fn noconsec [in out]
    (if (empty? in)
      out
      (if (= (first in) (first (rest in)))
        (noconsec (rest in) out)
        (noconsec (rest in) (conj out (first in))))
      )
    )
  % [])