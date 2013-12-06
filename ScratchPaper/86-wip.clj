; hmm
; well
; 3*3 = 9
; 9*9 = 81
; 8*8 + 1*1 = 65
; 6*6 + 5*5 = 61
; 6*6 + 1*1 = 37
; you know what I should write something to do this for me

; break into chars
(defn to-chars [n]
  (reduce #(conj %1 (Character/getNumericValue %2)) [] (str n)))

; whee
(defn happy-step [n]
  (reduce #(+ %1 (* %2 %2)) 0 (to-chars n)))
  
; generate an infinite lazy sequence
(defn gen-happy [n]
  (letfn [(to-chars [n] (reduce #(conj %1 (Character/getNumericValue %2)) [] (str n)))
          (happy-step [n] (reduce #(+ %1 (* %2 %2)) 0 (to-chars n)))]
  (cons n (lazy-seq (gen-happy (happy-step n))))))

; Oh, it loops over 4 (take 100 (gen-happy 3))
; loop sequence is 37 58 145...etc etc
; 2 loops over 4 (duh)
; 7 goes to 1
; 999912 is happy, huh.
; 8 loops over 4
; Is there a "Min steps to 4"?
; Uh, not sure, but 100 seems *more* than enough, from scattershot testing.

; Okay, take the sequence, reduce to a set, and if it has 1, it's happy
(fn is-happy [n]
  (letfn [(to-chars [n] (reduce #(conj %1 (Character/getNumericValue %2)) [] (str n)))
          (happy-step [n] (reduce #(+ %1 (* %2 %2)) 0 (to-chars n)))
          (gen-happy [n] (cons n (lazy-seq (gen-happy (happy-step n)))))]
    (contains? (reduce #(conj %1 %2) #{} (take 100 (gen-happy n))) 1)))