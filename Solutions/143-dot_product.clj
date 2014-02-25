;;;; 143 - dot product (2 lines)
(fn __ [c1 c2]
  (reduce + (map * c1 c2)))