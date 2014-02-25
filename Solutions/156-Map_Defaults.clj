;;;; 156 - Map Defaults (2 lines)
(fn __ [val coll]
  (apply hash-map (interleave coll (repeat (count coll) val))))