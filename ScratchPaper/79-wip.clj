; This will expand the line so it can be mapped to the next.
(defn expand [v n]
  (if (coll? (last v))
    (conj (pop v) (conj (last v) n) [n])
    [[n] [n]]))

; Testing.
(= [[3][3]] (reduce expand [] [3]))
(= [[5][5 7][7]] (reduce expand [] [5 7]))
(= [[15][15 15][15 14][14 12][12]] (reduce expand [] [15 15 14 12]))

; kay we should really wrap it up like so
(defn expand [coll]
  (reduce
    (fn [v n]
      (if (coll? (last v))
        (conj (pop v) (conj (last v) n) [n])
        [[n] [n]]))
    []
    coll))

; Function to produce the next collection
(defn next-level [top-coll bottom-coll]
  (map #(+ (apply min %1) %2) top-coll bottom-coll))

; Testing
(= [15 15 14 12](next-level [[6] [6 14] [14 10] [10]] [9 9 4 2]))
(= [15 15 14 12] (next-level (expand [6 14 10]) [9 9 4 2]))
(= [5 7] (next-level (expand [3]) [2 4]))
; Sweet, it works!

; Now to tie it all together
(defn min-tri [coll]
  (apply min (reduce #(next-level (expand %1) %2) coll)))

; And it works!
(= 7 (min-tri '([1] [2 4] [5 1 4] [2 3 4 5])))
(= 20 (min-tri '([3] [2 4] [1 9 3] [9 9 2 4] [4 6 6 7 8] [5 7 3 5 1 4])))

; Now to reformat it for 4Clojure
(fn min-tri [coll]
  (letfn [(expand [coll]
           (reduce
             (fn [v n]
               (if (coll? (last v))
                 (conj (pop v) (conj (last v) n) [n])
                 [[n] [n]]))
             []
             coll))
         (next-level [t-c b-c] (map #(+ (apply min %1) %2) t-c b-c))]
  (apply min (reduce #(next-level (expand %1) %2) coll))))
  
; Finally, prettier printing
(fn min-tri [coll]
  (letfn [(expand [coll]
                  (reduce (fn [v n] 
                            (if (coll? (last v)) 
                              (conj (pop v) (conj (last v) n) [n]) 
                              [[n] [n]]))
                          []
                          coll))
         (next-level [t-c b-c] 
                     (map #(+ (apply min %1) %2) t-c b-c))]
  (apply min (reduce #(next-level (expand %1) %2) coll))))