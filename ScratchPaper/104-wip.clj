; Okay, so we basically just go down the list, dividing and modding, yes?
; ...oooor, we could start with the right. Then we don't have to check the thousands, five-hundreds, etc, down 'till we reach the single digits if somebody puts in 9.

; We can do the string thing:
(fn split-digits [n]
  (map #(Integer/parseInt (str %)) (str n)))
  
(
(fn process-ones [n]
  (cond
    (<= n 3) (apply str (take n (repeat "I")))
    (= n 4) "IV"
    (< n 9) (apply str "V" (take (- n 5) (repeat "I")))
    (= n 9) "IX"))
7)

(
(fn process-tens [n]
  (cond
    (<= n 3) (apply str (take n (repeat "X")))
    (= n 4) "XL"
    (< n 9) (apply str "L" (take (- n 5) (repeat "X")))
    (= n 9) "XC"))
7)

; ...we could combine them to this:
(
(fn process-place [n one-sym five-sym ten-sym]
  (cond
    (<= n 3) (apply str (take n (repeat one-sym)))
    (= n 4) (str one-sym five-sym)
    (< n 9) (apply str five-sym (take (- n 5) (repeat one-sym)))
    (= n 9) (str one-sym ten-sym)))
7 "X" "L" "C")
; The drop-through means if you pass in 0 for n, it returns...empty. Turns out nicely, eh?

(
(fn write-roman [n]
  (let [split-digits 
          (fn [n] (map #(Integer/parseInt (str %)) (str n)))
        digits (into [] (reverse (split-digits n)))
        process-place
          (fn [n one-sym five-sym ten-sym]
            (cond
              (= n nil) ""
              (<= n 3) (apply str (take n (repeat one-sym)))
              (= n 4) (str one-sym five-sym)
              (< n 9) (apply str five-sym (take (- n 5) (repeat one-sym)))
              (= n 9) (str one-sym ten-sym)))]
    (str (process-place (get digits 3) "M" "M" "M")
         (process-place (get digits 2) "C" "D" "M")
         (process-place (get digits 1) "X" "L" "C")
         (process-place (get digits 0) "I" "V" "X"))))
3999)