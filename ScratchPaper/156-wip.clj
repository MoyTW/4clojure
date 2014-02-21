;; uuuh, interpose? Or interleave since interpose doesn't put at end.

(fn __ [val coll]
  (apply hash-map (interleave coll (repeat (count coll) val))))