;; 4Clojure Problem 119. Win at Tic-Tac-Toe
;; url: http://www.4clojure.com/problem/119
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