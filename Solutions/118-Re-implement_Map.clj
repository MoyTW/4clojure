;; 4Clojure Problem 118. Re-implement Map
;; url: http://www.4clojure.com/problem/118
(fn my-map [pred coll]
  (let [step (fn [p c]
                 (when-let [s (seq c)] ; when (seq c) is true, let s = (seq c), else return nil
                   (cons (pred (first s)) 
                         (my-map p (rest s)))))]
    (lazy-seq (step pred coll))))