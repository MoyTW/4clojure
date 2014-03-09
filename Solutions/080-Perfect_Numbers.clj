;; 4Clojure Problem 80. Perfect Numbers
;; url: http://www.4clojure.com/problem/80
(fn [n] 
  (= n (apply 
        + 
        (filter #(= 0 (mod n %)) 
                (rest (take n (range)))))))