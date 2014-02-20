;;;; -----=====***** 138 (63 lines, 63 total) *****=====-----
(fn __ [d m]
  (let [r-angle (- (/ Math/PI 4))
        r-pt 
          (fn r-pt [[x y]]
            (cond
              (= -3 x y) [-3 0]
              (= 3 x y) [3 0]
              (and (= -3 x) (= 3 y)) [0 3]
              (and (= 3 x) (= -3 y)) [0 -3]
              :else [(int (- (* x (Math/cos r-angle)) (* y (Math/sin r-angle))))
                     (int (+ (* x (Math/sin r-angle)) (* y (Math/cos r-angle))))]))
        lower-right
          (fn lower-right [square additions]
            (let [size (count square)
                  [_ bottom-range] (split-at size additions)]
              (conj (into [] (map conj square additions)) 
                    (into [] (reverse bottom-range)))))
        upper-left
          (fn upper-left [square additions]
            (let [size (count square)
                  [left-range top-range] (split-at size additions)]
              (vec (cons (vec top-range)
                         (map #(vec (cons %1 %2)) (reverse left-range) square)))))
        next-layer
          (fn next-layer [square additions]
            (if (= (mod (count additions) 4) 3)
                (lower-right square additions)
                (upper-left square additions)))
        make-squares
          (fn make-squares [inputs]
            (reduce next-layer [(first inputs)] (rest inputs)))
        make-seq
          (fn make-seq [begin end]
            (let [ints (take-while #(<= % end) (iterate #(* % %) begin))
                  as-chars (mapcat str ints)
                  square-area (first (drop-while #(< % (count as-chars)) 
                                                 (reductions + (iterate #(+ 2 %) 1))))
                  un-partitioned (concat as-chars (repeat (- square-area (count as-chars)) \*))]
              (loop [s un-partitioned n 1 out []]
                (if (seq s)
                    (let [[pre post] (split-at n s)]
                      (recur post (+ n 2) (conj out (vec pre))))
                    out))))
        pad-with-spaces
          (fn pad-with-spaces [s]
            (vec (interpose (vec (repeat (+ 2 (count s)) nil)) (map #(vec (interpose nil %)) s))))
        resolve-square
          (fn resolve-square [s]
            (let [half (int (/ (count s) 2))
                  map-coords (for [x (range 0 (count s)) 
                                   y (range 0 (count s))]
                               [x y])
                  to-traditional-coords (vec (reverse s))
                  new-vec (reduce (fn [m [x y]]
                                    (if-let [sym (get-in to-traditional-coords [x y])]
                                      (let [new-coords (r-pt [(- x half) (- y half)])
                                            new-indices (map #(+ half %) new-coords)]
                                        (assoc-in m new-indices sym))
                                      m))
                                  (vec (repeat (count s) (vec (repeat (count s) \space))))
                                  map-coords)]
              new-vec))]
    (apply map str (resolve-square (pad-with-spaces (make-squares (make-seq d m)))))))