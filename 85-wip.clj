; Power set, huh. I remember the term but nothing about it.
; Okay. How about this. Take the set, add it to the power set, and then split it into n sets, each lacking one of the members of the parent set.
; For each of these n sets, repeat until you reach the empty set.

; By hand:
; #{1 2 3}
; Sets: #{1 2} #{2 3} #{1 3} POWER: #{#{1 2 3}}
; Sets: #{2} #{1} #{3} #{2} #{3} #{1} POWER: #{#{1 2 3} #{1 2} #{2 3} #{1 3}}
; Sets: #{} #{} #{} #{} #{} #{} POWER: #{#{1 2 3} #{1 2} #{2 3} #{1 3} #{2} #{1} #{3}}

; This seems like it would do what we need.
; Given a set, generate the next level of sets (in a set)
(
(defn break-up [st]
  (reduce #(conj %1 (disj st %2)) #{} st))
#{1 2 3})

; Do that to a while line of sets
(
(defn next-line [st]
  (reduce #(apply conj %1 (break-up %2)) #{} st))
#{#{1 2 3}})

(
(defn power-set [trgt-st]
  (loop [st #{trgt-st} pwr-st #{}]
    (if (empty? st) pwr-st
      (recur (next-line st) (apply conj pwr-st (next-line st))))))
#{1 2 3})
;hmm

; there we go
(
(defn power-set [trgt-st]
  (loop [st #{trgt-st} pwr-st #{trgt-st}]
    (if (= st #{#{}}) pwr-st
      (recur (next-line st) (apply conj pwr-st (next-line st))))))
#{1 2 3})

; For 4Clojure
(fn power-set [trgt-st]
  (letfn [(break-up [st]
                    (reduce #(conj %1 (disj st %2)) #{} st))
          (next-line [st]
                     (reduce #(apply conj %1 (break-up %2)) #{} st))]
    (loop [st #{trgt-st} pwr-st #{trgt-st}]
      (if (= st #{#{}}) pwr-st
        (recur (next-line st) (apply conj pwr-st (next-line st)))))))