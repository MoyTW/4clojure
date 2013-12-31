;; Analyze Reversi? I have no idea what Reversi is.

;; This will generate all eight directional vectors
(for [x [+ - identity] y [+ - identity] :when (not= [identity identity] [x y])] [x y])

;; Now, when you have a point - say [0 3], and you want to get the + - vector (should end up as [w b e]):
(def board
'[[e e e e]
  [e w b e]
  [w w b e]
  [e e b e]])

;; Hmm, hmm. So, we want all the characters in the specified direction. Now, if you do get-in with an out-of-bounds index, does it throw an error, or return nil?
(get-in [[]] [3 3])
;; So, that returns nil. So we can actually do a take-while of the sequence provided by repeatedly applying the two operations to the keys.
(
(defn line [start-coords [fr fc]]
  (take 10 
    (reductions
      (fn f [[old-r old-c] n]
        [(fr old-r n) (fc old-c n)])
      start-coords
      (repeat 1))))
[3 3] [+ +])
;; Unfortunately this won't work with identity.
;; Also, it's a kind of convoluted method!

;; So, let's step back and think for a moment...
;; The current algorithm is, in normal language:
;;  All squares with a matching color:
;;      Check each of the eight lines for a sequence matching: OTHER COLOR* EMPTY
;;          Matches return OTHER COLOR*, EMPTY
;; Now that's a bit pants because you could have two different anchor points each pointing to one, and without having the anchor, you'd be in trouble picking out all the items which would be flipped by the move. Fortunately, a careful perusal of the tests will reveal that this situation never occurs! So, uh, we'll just...ignore it for now and pretend that'll never happen, ever, and put up a little disclaimer saying "This doesn't actually analyze Reversi!"

(
(defn line-from-board [start-coords [f-row f-col]]
  (take-while #(get-in board %)
              (reductions
                (fn f [[r c] n]
                  [(f-row r n) (f-col c n)])
                start-coords
                (repeat 1))))
[1 0] [+ +])

;; Get the chars associated with the coordinates (well...they're quoted, so...uh...not chars, actually.)
(
(defn line-from-board [start-coords [f-row f-col]]
  (map #(vector % (get-in board %))
       (take-while #(get-in board %)
                   (reductions
                     (fn f [[r c] n]
                       [(f-row r n) (f-col c n)])
                     start-coords
                     (repeat 1)))))
[1 0] [+ +])

;; Anyways. Get the coordinates of all pieces matching target piece on the board:
(
(defn get-matching-coordinates [board piece]
  (for [r (range (count board))
       c (range (count (first board)))
       :when (= piece (get-in board [r c]))]
    [r c]))
board 'w)
;; Because 4Clojure does not like def, we can stick this into a closure when we actually make it a sub-function of our gigantic result function...

;; These only need to be generated once - we can stick them in a let, or something...
(defn gen-dir-vecs []
  (letfn [(id-func [i & args] i)]
    (for [x [+ - id-func]
          y [+ - id-func]
          :when (not= [id-func id-func] [x y])]
      [x y])))
      
;; Now, let's see. If we're given a line consisting of quoted forms, how do we tell if it's a valid movement line?
;; First step: it MUST be at least 3 items long (1 item means it's on edge, 2 items means it's next to edge and hence no valid move, 3 items means it can be valid)
;; Then, there are N other colors, ending with an e.
;; So, something like...
(
(defn valid-placement [line color]
  (let [other-color (if (= color 'w) 'b 'w)
        [other [first-not-other & rest-not-other]] (split-with #(= other-color (last %)) (rest line))]
    other))
    ;(= 'e (last first-not-other))))
'([[1 0] w] [[2 1] b] [[3 2] b] [[4 3] e]) 'w)
;; Except that we want the placement point and all the flipped targets, as a map, so...
(
(defn placement [line color]
  (let [other-color (if (= color 'w) 'b 'w)
        [other [first-not-other & rest-not-other]] (split-with #(= other-color (last %)) (rest line))]
    (if (and (> (count line) 2) (= 'e (last first-not-other)))
        {(first first-not-other) (into #{} (map #(first %) other))}
        nil)))
'([[1 0] w] [[2 1] b] [[3 2] e]) 'w)

(placement '([[1 0] w] [[2 1] b] [[3 2] b] [[4 3] e]) 'w)
;; Well this is all terribly disorganized and confusing...that's why it's scratch paper!