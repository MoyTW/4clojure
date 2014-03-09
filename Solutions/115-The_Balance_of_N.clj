;; 4Clojure Problem 115. The Balance of N
;; url: http://www.4clojure.com/problem/115
(fn compare-side-digits [x]
  (let [s (map #(Integer. (str %)) (str x))
        l-c (int (/ (count s) 2)) 
        r-c (Math/ceil (/ (count s) 2))]
    (= (apply + (take l-c s)) 
       (apply + (drop r-c s)))))