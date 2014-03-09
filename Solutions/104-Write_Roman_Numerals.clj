;; 4Clojure Problem 104. Write Roman Numerals
;; url: http://www.4clojure.com/problem/104
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