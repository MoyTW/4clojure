;; 4Clojure Problem 92. Read Roman numerals
;; url: http://www.4clojure.com/problem/92
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