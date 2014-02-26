;; Hey, currying. I remember what that is! So.
;; Lessee.

(fn [a] (fn [b] (fn [c] (fn [d] (+ a b c d)))))

(fn __ [f]
  (fn [& args]
    (loop [r f args args]
      (if (seq args)
          (recur (r (first args)) (rest args))
          r))))
        
((__ (fn [a] (fn [b] (* a b))))
       5 5)