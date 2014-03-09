;; 4Clojure Problem 31. Pack a Sequence
;; url: http://www.4clojure.com/problem/31
#(
  (fn noconsec [in out]
    (if (empty? in)
      (reverse out)
      (if (= (first in) (first (first out)))
        (noconsec 
		  (rest in)
          (conj (rest out) (conj (first out) (first in))))
        (noconsec (rest in) (conj out (list(first in)))))
    )
  )
  (rest %) (list (list (first %)))
)