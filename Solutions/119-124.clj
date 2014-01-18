;;;; -----=====***** 119 (18 lines, 18 total) *****=====-----
; With thanks to:
; https://github.com/ardumont/org/blob/master/clojure/4clojure-73-analyze-a-tic-tac-toe-board.org
(fn winning-moves [piece board]
  (let [coords (for [x (range 0 3)
                     y (range 0 3)]
                 [x y])
        placements (filter #(= :e (get-in board %)) coords)
        won? (fn [b]
               (letfn [(w [[[a b c]
                            [d e f]
                            [g h i]] p] (or (= p a b c)
                                        (= p d e f)
                                        (= p g h i)
                                        (= p a d g)
                                        (= p b e h)
                                        (= p c f i)
                                        (= p a e i)
                                        (= p c e g)))]
                 (if (w b piece) true)))]
    (into #{} (filter #(won? (assoc-in board % piece)) placements))))

;;;; -----=====***** 120 (5 lines, 23 total) *****=====-----
(fn ssqd [coll]
  (letfn [(smaller? [n]
            (let [digits (map (fn [x] (Integer. (str x))) (into [] (str n)))]
              (< n (reduce #(+ %1 (* %2 %2)) 0 digits))))]
    (count (filter smaller? coll))))

;;;; -----=====***** 121 (15 lines, 38 total) *****=====-----
(fn return-eval [l]
  (fn e-map [m]
    (letfn [(e-p [p]
              (cond
                (list? p) (e p)
                (number? p) p
                :else (m p)))
            (e [[op & args]]
              (let [e-args (map e-p args)]
                (cond
                  (= op '*) (apply * e-args)
                  (= op '+) (apply + e-args)
                  (= op '-) (apply - e-args)
                  (= op '/) (apply / e-args))))]
      (e l))))

;;;; -----=====***** 122 (1 line, 39 total) *****=====-----
#(Integer/parseInt % 2)

;;;; -----=====***** 122 (0 lines, 39 total) *****=====-----
;; No such problem!

;;;; -----=====***** 124 (29 lines, 68 total) *****=====-----
(fn analyze-reversi [board piece]
  (let [other-piece (if (= piece 'w) 'b 'w)
        dir-vecs (letfn [(id-func [i & args] i)]
                   (for [r [+ - id-func]
                         c [+ - id-func]
                         :when (not= [id-func id-func] [r c])]
                     [r c]))
        matching-coordinates (for [r (range (count board))
                                   c (range (count (first board)))
                                   :when (= piece (get-in board [r c]))]
                               [r c])
        line-from (fn [start-coords [f-r f-c]]
                    (map #(vector % (get-in board %))
                         (take-while #(get-in board %)
                                     (reductions
                                       (fn f [[r c] n]
                                         [(f-r r n) (f-c c n)])
                                       start-coords
                                       (repeat 1)))))
        placement (fn [line]
                    (let [[other [first-not-other & rest-not-other]] (split-with #(= other-piece (last %)) (rest line))]
                      (if (and (> (count line) 2) (> (count other) 0) (= 'e (last first-not-other)))
                          {(first first-not-other) (into #{} (map #(first %) other))}
                          nil)))
        ]
    (apply merge (filter map?
                         (for [coords matching-coordinates
                               dv dir-vecs]
                           (placement (line-from coords dv)))))))