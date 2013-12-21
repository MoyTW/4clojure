;; Ah, so we're back to the whole "Convert to a string, pull the digits out, convert back" thing. I swear, we've done this, like, five times now? I probably have code sitting somewhere back there, but let's do this without going back and hunting it down.
;; I'd like to say this is because of a commitment to learning and doing things from memory but it's actually because I think it'll be more work to find it than to just rewrite it.
(
(fn ssqd [coll]
  (letfn [(smaller? [n]
            (let [digits (map (fn [x] (Integer. (str x))) (into [] (str n)))]
              (< n (reduce #(+ %1 (* %2 %2)) 0 digits))))]
    (count (filter smaller? coll))))
(range 10))