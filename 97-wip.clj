; This is like the other one, the lowest path one.
; See, it looks like this:
; 1
; 1 1
; 1 2 1
; 1 3 3 1
; 1 4 6 4 1
; So basically, each iteration, you do above[your-index] + above[your-index - 1]
; ...actually, that's...pretty much a reduce, there.

(fn next-row [row]
  (reduce (fn [[out l] n] 
            [(conj out (+ l n)) n]) 
          [[] 0] 
          row))
[1 3 3 1])
; waaait a second
; ...l is always 1, because the triangle always ends with 1. Ha! Okay.

(fn next-row [row]
  (let [n-row (reduce (fn [[out l] n] 
                        [(conj out (+ l n)) n]) 
                      [[] 0] 
                      row)]
      (conj (first n-row) (last n-row))))

; Aaaand we want to run next-row n times. It's basically iteration.
(defn next-row [row]
  (let [n-row (reduce (fn [[out l] n] 
                        [(conj out (+ l n)) n]) 
                      [[] 0] 
                      row)]
      (conj (first n-row) (last n-row))))
(take 1 (iterate next-row [1]))
(nth (iterate next-row [1]) 5)

(defn nth-row [n]
  (letfn [(next-row [row]
            (let [n-row (reduce (fn [[out l] n] 
                                  [(conj out (+ l n)) n]) 
                                 [[] 0] 
                                 row)]
               (conj (first n-row) (last n-row))))]
    (nth (iterate next-row [1]) (dec n))))
(nth-row 1)
(nth-row 2)
; Looks like that's it. Easy 'nuff.