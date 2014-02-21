;; Hey it's operator overloading all over again.

(fn __ [op item-one item-two]
  (cond
    (= (op item-one item-two) (op item-two item-one)) :eq
    (op item-one item-two) :lt
    :else :gt))