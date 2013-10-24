; So...XOR.
; Convert to a set, and if we've got both...

(
(fn long-xor [& args]
  (if (= (count (apply conj #{} args)) 2) true false))
true false)