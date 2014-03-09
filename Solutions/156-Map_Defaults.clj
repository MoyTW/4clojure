;; 4Clojure Problem 156. Map Defaults
;; url: http://www.4clojure.com/problem/156
(fn __ [val coll]
  (apply hash-map (interleave coll (repeat (count coll) val))))