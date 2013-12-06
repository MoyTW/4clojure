; I've got some reading to do...
; Oh, that was pretty easy, actually.

; So, for the more-legs set #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}
; cat has more legs than man, man has more legs than snake, spider has more legs than cat
; therefore cat has more legs than man and snake
; spider has more legs than cat, man, and snake

; Okay, now how do I do this thing...
; On reflection it's kind of similar to the thing I was doing before - it's a chain:
; [spider -> cat -> man -> snake]
; but it's one-way only.
; Also it's not assured to form one chain (see progeny example)

; Well, okay, here's what we could do.
; Pull it into a map. For each key, generate the pair, and if the value is in the map, go down and generate that pair, and so on and so on.

; To map
(defn to-map [st]
  (reduce #(assoc %1 (first %2) (second %2)) {} st))

; for one member
(
(defn single [out-set next-key mp]
  (loop [os out-set k (first next-key)]
    (let [v (get mp k)]
      (if (or (= v nil) (= k nil)) os
        (recur (conj os [(first next-key) v]) v)))))
#{} ["spider" "cat"] {"spider" "cat", "man" "snake", "cat" "man"})
              
; does closure
(
(fn closure [st]
  (let [mp (reduce #(assoc %1 (first %2) (second %2)) {} st)]
    (reduce
      (fn close-over [out-set next-key]
        (loop [os out-set k (first next-key)]
          (let [v (get mp k)]
            (if (or (= k nil) (= v nil)) os
              (recur (conj os [(first next-key) v]) v)))))
      #{} mp)))
#{["cat" "man"] ["man" "snake"] ["spider" "cat"]})

(reduce (fn [x y] (do (println y) x)) {"spider" "cat", "man" "snake", "cat" "man"})