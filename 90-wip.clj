; Hmm.
; Now if I were doing this in C# (or Python or Java) I'd simply iterate over
; the two sets in a nested for/foreach loop and spit out the appropriate cross
; products. Gotta find a slightly different way to do it here, though.

(
(fn cross-product [lset rset]
  (reduce
    (fn r [s n]
      (apply conj s (reduce #(conj %1 [%2 n]) #{} lset)))
    #{} 
    rset))
#{"ace" "king" "queen"} #{"spade" "heart" "diamond" "club"})