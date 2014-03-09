;; 4Clojure Problem 73. Analyze a Tic-Tac-Toe Board
;; url: http://www.4clojure.com/problem/73
; This is HORRIFYING
(fn check-lines [coll]
  (loop [rng (take 3 (range))]
    (let [three-in-row (fn three-in-row [coll]
                         (let [result (reduce #(if (= %1 %2) %1 nil) (first coll) coll)] 
                           (if (or (= result :o) (= result :x)) 
                             result 
                             nil)))]
      ; This handles diagonals
      (if (= '() rng)
        (let [r-v (three-in-row [(nth (nth coll 0) 0) (nth (nth coll 1) 1) (nth (nth coll 2) 2)])
              l-v (three-in-row [(nth (nth coll 0) 2) (nth (nth coll 1) 1) (nth (nth coll 2) 0)])]
          (if r-v
            r-v
            (if l-v
              l-v
              nil)))
        ; This handles horizontal/verticals
        (let [n (first rng)
              v-val (three-in-row (nth coll n))
              h-val (three-in-row (reduce #(conj %1 (nth %2 n)) [] coll))]
          (if v-val
            v-val
            (if h-val
              h-val
              (recur (rest rng)))))))))