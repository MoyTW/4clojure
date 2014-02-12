;;;; 119 - Win at Tic-Tac-Toe
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/119-wip.clj
;; Original:
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
;;   So this uses the same format as #73, except that instead of just analyzing
;; whether a player has won you have to generate the set of winning moves. This
;; is actually super duper easy if you have the code to analyze the board
;; already - you just generate the set of legal moves, given the board and the
;; piece, and then you check which of them win, and then you're done! It's
;; almost trivial.
;;   Here, the function to check the board is placed in won?. It's a weird
;; mishmash of destructuring and equals checks because way back when, when I
;; went and reviewed 73, I discovered this particularly ingeniously hacky
;; solution and decided immediately it was the best way of solving the "Has
;; anybody won this game of tic-tac-toe?" ever. Unless somebody shows me a
;; 1-liner, I think it'll remain so, purely for the ingenious nature of the
;; thought displayed, but you could really put any old method of analyzing the
;; board in.
;;   Anyways. The other moving parts are coords, which is the list of all
;; possible placements [0 0] [0 1] ... [2 2], placements, which are coords for
;; which there is an empty spot, and the final filter statement. The filter
;; statement returns all the placements where placing the selected move into the
;; board would result in a win.
;;   One thing to note here is that the function in won? has a letfn inside of
;; it, and that it's also kind of atrociously spaced. We can fix that, though!
;; Let's muck around a bit and compress things:
(fn winning-moves [piece board]
  (let [coords (for [x (range 0 3)
                     y (range 0 3)]
                 [x y])
        placements (filter #(= :e (get-in board %)) coords)
        won? (fn [[[a b c]
                   [d e f]
                   [g h i]]] 
               (or (= piece a b c)
                   (= piece d e f)
                   (= piece g h i)
                   (= piece a d g)
                   (= piece b e h)
                   (= piece c f i)
                   (= piece a e i)
                   (= piece c e g)))]
    (into #{} (filter #(won? (assoc-in board % piece)) placements))))
;;   If you rip out the actual won? function, the whole thing boils down to a
;; nice compact six lines. Pretty decent, actually! The refactor basically just
;; made won? not a horrible mess of nested functions, but didn't touch anything
;; outside because I'm actually feeling pretty good about that.
;;   I wonder what it would look like if I used my actual code from 73, though?
(fn winning-moves [piece board]
  (let [coords (for [x (range 0 3)
                     y (range 0 3)]
                 [x y])
        placements (filter #(= :e (get-in board %)) coords)
        check-lines
          (fn [coll]
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
        won? (fn [board] (= (check-lines board) piece))]
    (into #{} (filter #(won? (assoc-in board % piece)) placements))))
;;   I'm suddenly very sorry I asked. Even my updated code for #73 is pretty bad!
;; I mean, come on, look at this - sure it's a little shorter but it is not
;; pretty:
(fn winning-moves [piece board]
  (let [coords (for [x (range 0 3)
                     y (range 0 3)]
                 [x y])
        placements (filter #(= :e (get-in board %)) coords)
        check-lines
          (fn [coll]
            (let [check-row (fn [coll]
                              (if (and (= (coll 0) (coll 1) (coll 2))
                                       (not= (coll 0) :e))
                                  (coll 0)
                                  nil))
                  build-vectors (fn [coll]
                                  (let [rng (take 3 (range))
                                        pull-hori (fn [n] (reduce #(conj %1 (%2 n)) [] coll))
                                        straight-vectors (mapcat #(vector (coll %) (pull-hori %)) rng)]
                                    (conj straight-vectors
                                          [((coll 0) 0) ((coll 1) 1) ((coll 2) 2)]
                                          [((coll 0) 2) ((coll 1) 1) ((coll 2) 0)])))
                  vectors (build-vectors coll)]
              (loop [vectors vectors]
                (when (seq vectors)
                  (if-let [result (check-row (first vectors))]
                          result
                          (recur (rest vectors)))))))
        won? (fn [board] (= (check-lines board) piece))]
    (into #{} (filter #(won? (assoc-in board % piece)) placements))))
;;   Hmm okay let's just tinker with this one some, I mean, there's a nil in the
;; if that can go and we could pull these functions out of that double-let...
(fn winning-moves [piece board]
  (let [coords (for [x (range 0 3)
                     y (range 0 3)]
                 [x y])
        placements (filter #(= :e (get-in board %)) coords)
        check-row 
          (fn [coll]
            (if (and (= (coll 0) (coll 1) (coll 2))
                     (not= (coll 0) :e))
                (coll 0)))
        build-vectors
          (fn [coll]
            (let [rng (take 3 (range))
                  pull-hori (fn [n] (reduce #(conj %1 (%2 n)) [] coll))
                  straight-vectors (mapcat #(vector (coll %) (pull-hori %)) rng)]
              (conj straight-vectors
                    [((coll 0) 0) ((coll 1) 1) ((coll 2) 2)]
                    [((coll 0) 2) ((coll 1) 1) ((coll 2) 0)])))
        check-lines
          (fn [coll]
            (loop [vectors (build-vectors coll)]
              (when (seq vectors)
                    (if-let [result (check-row (first vectors))]
                            result
                            (recur (rest vectors))))))
        won? (fn [board] (= (check-lines board) piece))]
    (into #{} (filter #(won? (assoc-in board % piece)) placements))))
;;   Eh, it won't be winning any beauty contests this way, either, will it? At
;; least it's considerably improved over the first one, though!

;;;; 120 - Grouped with 122 because it's short and 122's long

;;;; 121 - Universal Computation Engine
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/121-wip.clj
;; Original:
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
;;   I remember having to do this in C++ in college. My friend and I did this
;; side-by-side but at some point I mistyped something and oh! everything broke!
;; except we were working side-by-side, so why was his working and mine not? We
;; pored over the code, checking the logic over and over again. Turns out the
;; bug was in my I/O, which I had assumed I had because I totally know I/O this
;; simple console I/O stuff, right? Bug couldn't be in there, right?
;;   Anyways.
;;   So, the way I do this is...intricate. Hmm. So, what I'm doing is using an
;; outside function to close over the returned function, which carries l - the
;; list - with it. Then, when it's called, it does a recursive resolution with a
;; pair of mutually recursive functions, e-p and e, which I'm wishing I'd named
;; better. e-p basically resolves the type of the input, dispatching it to e
;; (evaluation, I assume?) if it's a list, returning its proper value if it's a
;; number, or looking it up in the map. e resolves the actual mathamatical
;; operations on the terminals, calling e-p to get the number values of all the
;; things in the list, and then applying the operation to them.
;;   Everything's a one-letter variable, which is deeply disappointing to
;; present-me, but it's not wrong. I guess I could rewrite them with spelled out
;; names but as it turns out this approach is barking up the wrong tree! Now
;; being older and wiser and more versed in the ways of the Clojure, how about
;; we use that crazy homoionicity thing?
;;   Did I spell that right? I don't think I spelled that right.
;;   Anyways. We can use quoting and unquoting and eval to do this in a mere
;; few lines! See:
(fn [l]
  (fn [m]
    (eval `(let [~@(vector (into [] (keys m))) ~@(vector (into [] (vals m)))] ~l))))
;;   Unfortunately eval isn't allowed in 4Clojure so, uh, yeah. 4Clojure. :( I
;; love you but this is a serious oversight in the overall coverage of the
;; Clojure language.
;;   So back to rewriting the original function! Well, that cond seems, hmm, hold
;; up. Yeah, how about:
(fn return-eval [l]
  (fn eval-map [m]
    (letfn [(eval-part [part]
              (cond
                (list? part) (do-eval part)
                (number? part) part
                :else (m part)))
            (do-eval [[op & args]]
              (let [eval-args (map eval-part args)]
                (condp = op
                  '* (apply * eval-args)
                  '+ (apply + eval-args)
                  '- (apply - eval-args)
                  '/ (apply / eval-args))))]
      (do-eval l))))
;;   Actually wait, that's more like a selection, there. Or a mapping. We can
;; condense it quite a bit by selecting from a map:
(fn return-eval [l]
  (fn eval-map [m]
    (letfn [(eval-part [part]
              (cond
                (list? part) (do-eval part)
                (number? part) part
                :else (m part)))
            (do-eval [[op & args]]
              (let [eval-args (map eval-part args)
                    o ({'* * '+ + '- - '/ /} op)]
                (apply o eval-args)))]
      (do-eval l))))
;;   I'm sure there's a more elegant way to do it than a mutually recursive
;; function pair - heck, there's probably a single mutually recursive function
;; that'll walk down and eval it all elegant-like - but I'm satisfied with this
;; for the moment.