; What is the algorithm for reading Roman numerals?
; Break it up
; What we basically do is require look-aheads for specific pairs of sequencs (IV, IX, XL, XC, CD, CM). Can either save last and commit late, or jump forwards.
; Actually we can handle that really easily with a reduce to a 2-element data structure (running sum and holding area)
; Function will consume one-by-one:
;  if I, X, C:
;    if already holding matching, add to sum and continue
;    if forms sequence, add sequence, nil holding
;    if other and no sequence, add value, nil holding

; We can use a map to, uh, map characters to values
(def numerals {"I" 1, "IV" 4, "IX", 9, "V" 5, "X" 10, "XL" 40, "XC" 90, "L" 50, "C" 100, "CD" 400, "CM" 900, "M" 1000})

; Function
(
(fn process-numeral [sum-hold n]
  (let [numerals {nil 0 "I" 1, "IV" 4, "IX", 9, "V" 5, "X" 10, "XL" 40, "XC" 90, "L" 50, "C" 100, "CD" 400, "CM" 900, "M" 1000}
        i-sum (first sum-hold)
        i-hold (last sum-hold)
        n-str (str i-hold n)]
    (condp contains? n-str
      #{"I" "X" "C"} [i-sum n-str]
      #{"II" "XX" "CC"} [(+ i-sum (get numerals i-hold)) n]
      #{"IV" "IX" "XL" "XC" "CD" "CM"} [(+ i-sum (get numerals n-str)) nil]
      [(+ i-sum (get numerals i-hold)) n])))
;[19 nil] "I")
;[19 "I"] "I")
[19 "X"] "I")

; Conversion
(
(fn to-arabic [s]
  (let [numerals {"" 0 "I" 1, "IV" 4, "IX", 9, "V" 5, "X" 10, "XL" 40, "XC" 90, "L" 50, "C" 100, "CD" 400, "CM" 900, "M" 1000}
  p-n (fn [sum-hold n]
  (let [i-sum (first sum-hold)
        i-hold (last sum-hold)
        n-str (str i-hold n)]
    (condp contains? n-str
      #{"I" "X" "C"} [i-sum n-str]
      #{"II" "XX" "CC"} [(+ i-sum (get numerals i-hold)) n]
      #{"IV" "IX" "XL" "XC" "CD" "CM"} [(+ i-sum (get numerals n-str)) nil]
      [(+ i-sum (get numerals (str i-hold))) n])))]
      (let [pair (reduce p-n [0 nil] s)]
        (+ (first pair) (get numerals (str (last pair)))))))
"XIV")

; "pretty" print and bugfixed (missed (str))
(
(fn to-arabic [s]
  (let [numerals {"" 0 "I" 1, "IV" 4, "IX", 9, "V" 5, "X" 10, "XL" 40, "XC" 90,
        "L" 50, "C" 100, "CD" 400, "CM" 900, "D" 500, "M" 1000}
        p-n (fn [sum-hold n]
              (let [i-sum (first sum-hold)
                    i-hold (last sum-hold)
                    n-str (str i-hold n)]
                (condp contains? n-str
                  #{"I" "X" "C"} [i-sum n-str]
                  #{"II" "XX" "CC"} [(+ i-sum (get numerals (str i-hold))) n]
                  #{"IV" "IX" "XL" "XC" "CD" "CM"} 
                    [(+ i-sum (get numerals n-str)) nil]
                  [(+ i-sum (get numerals (str i-hold))) n])))]
    (let [pair (reduce p-n [0 nil] s)]
      (+ (first pair) (get numerals (str (last pair)))))))
"DCCCXXVII")