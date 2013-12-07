;; Okay. How about we split it in the middle, and then sum the two sides?
;; ...there's got to be a better way to do this than the string-trickery I've been doing.
;; Just Googled something similar; first hit on SO involves people using mod, but...no. String is better - mod requires you to loop over it.

(
(fn split-center [x]
  (let [s (map #(Integer. (str %)) (str x))
        l-c (int (/ (count s) 2)) 
        r-c (Math/ceil (/ (count s) 2))]
    [(take l-c s) (drop r-c s)]))
31213)

(
(fn compare-side-digits [x]
  (let [s (map #(Integer. (str %)) (str x))
        l-c (int (/ (count s) 2)) 
        r-c (Math/ceil (/ (count s) 2))]
    (= (apply + (take l-c s)) 
       (apply + (drop r-c s)))))
31213)