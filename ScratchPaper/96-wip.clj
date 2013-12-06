; Symmetric, huh. I assume we can expect well-formed input.
; ...okay. Can we just flip one side around and compare it to the other?

(
(fn reverse-btree [[val l-chld r-chld :as node]]
  (if (= node nil) nil
    [val (reverse-btree r-chld) (reverse-btree l-chld)]))
[2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil])

(fn check-sym [[val l-tree r-tree]]
  (letfn [(reverse-btree [[val l-chld r-chld :as node]]
           (if (= node nil) nil
             [val (reverse-btree r-chld) (reverse-btree l-chld)]))]
    (= l-tree (reverse-btree r-tree))))