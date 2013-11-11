; I think I already did something like this. Roman numerals?
; Anyways, I'm thinking just turn it into a string, split it up, and turn it back.

(fn split-digits [n]
  (map #(Integer/parseInt (str %)) (str n)))

; aaaand
(fn product-digits [x y]
  (map #(Integer/parseInt (str %)) (str (* x y))))