;; dot product?
(fn __ [c1 c2]
  (reduce + (map * c1 c2)))