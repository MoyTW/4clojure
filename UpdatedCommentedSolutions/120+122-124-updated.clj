;;;; 120 - Sum square of digits
;;; Scratch: https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/120-wip.clj
;; Original:
(fn ssqd [coll]
  (letfn [(smaller? [n]
            (let [digits (map (fn [x] (Integer. (str x))) (into [] (str n)))]
              (< n (reduce #(+ %1 (* %2 %2)) 0 digits))))]
    (count (filter smaller? coll))))
;; For some reason there seem to be a ton of the "break the number up into digits and do mostly trivial math on them" types of problems on 4Clojure. I wonder if there's a more mathamatical way to do it than my old standby "integer->string->map to integers."
;; Oh, wait, there is, duh. Mod and divide, pretty easy, right? Right, totally easy. We can rewrite this fairly easily - but first let's see if we can't touch up the existing code.
(fn ssqd [coll]
  (letfn [(smaller? [n]
            (let [digits (map #(Integer. (str %)) (into [] (str n)))
                  squares (map #(* % %) digits)]
              (< n (reduce + squares))))]
    (count (filter smaller? coll))))
;;   Hmm we could use ->> here:
(fn ssqd [coll]
  (letfn [(smaller? [n]
            (->> (str n)
                 (map str)
                 (map #(Integer. %))
                 (map #(* % %))
                 (reduce +)
                 (< n)))]
    (count (filter smaller? coll))))
;; You know, I'm not sure how to handle the formatting on those threaded maps there. On the one hand, three map calls in a row? On the other hand...well, what would smooshing them together look like?
(fn ssqd [coll]
  (letfn [(smaller? [n]
            (->> (str n)
                 (map #(* (Integer. (str %)) (Integer. (str %))))
                 (reduce +)
                 (< n)))]
    (count (filter smaller? coll))))
;; I'm sure there's a more elegant way of doing it, but I don't know, it looks too assymetrical for my tastes and besides that's kind of cramming too much into the anonymous function. Eh, these are all just formatting issues. Let's rewrite it using mod and division:
(fn ssqd [coll]
  (let [smaller? 
         (fn smaller? [n]
           (loop [sum 0 rest-n n]
             (if (zero? rest-n)
                 (< n sum)
                 (recur (+ sum (* (mod rest-n 10) (mod rest-n 10))) 
                        (int (/ rest-n 10))))))]
    (count (filter smaller? coll))))
;; Obviously the loop/recur here is kind of janky (especially that recur statement!) but it gets the job done. I'm sure it could be prettied up if need be. The idea's just to show an alternative, more math-based way of doing it than casting to strings and casting the constituent characters back to integers.

;;;; 122 - Read a binary number
;;; Scratch: https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/122-wip.clj
;; Original:
#(Integer/parseInt % 2)
;;   Hey, guess what? There's a function for that!
;;   If we wanted to be less cheesy:
(defn accumulate [[sum place] n]
  [(+ sum (* n place)) (* place 2)])
(defn parse-bstr [bstr]
  (->> (reverse bstr)
       (map #(Integer. (str %)))
       (reduce accumulate [0 1])
       (first)))
;; And, since 4Clojure doesn't very much like defn:
(fn parse-bstr [bstr]
  (let [accumulate (fn [[sum place] n]
                     [(+ sum (* n place)) (* place 2)])]
    (->> (reverse bstr)
         (map #(Integer. (str %)))
         (reduce accumulate [0 1])
         (first))))
;; We need the reverse because otherwise reduce will start at the front of the string, and oh dear, that's a problem! Conveniently the input is already in string form, so breaking it up into zeroes or ones is very easy. You could monkey around with trues, faleses, and ifs, but that would be needlessly complicating it. Once it's broken down into a vector of zeroes and ones, we just reduce over it, multiplying the next element by its place value and then summing. Simple, yes?

;;;; 123 - No such problem!

;;;; 124 - Analyze Reversi
;;; Scratch: https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/124-wip.clj
;; Original:
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
;; You know, I've completely forgotten what reversi is. Funny how that works, isn't it? Let's see how readable my code is.
;; other-piece is pretty simple. It returns the, uh, other piece - that is, if you pass in white it'll return black, and vice versa.
;;   aaand I'm lost. Okay, let me go and look at the problem again.
;;   Okay, so, now that I've had a quick refresher on what I'm trying to solve:
;; The placement function should really have a ?, because what it does it checks to see whether there's a winning placement for a particular board on a particular line. The main function generates all the possible lines radiating from already placed pieces, then checks to see whether any of those lines contain winning moves, and merges all the successful ones to get all the winning moves.
;; dir-vecs, which initially confused me, is actually very simple. It returns the vectors representing each of the eight directions - up, down, diagonals - as permutations of +, -, and id-func, which is like identity but instead of returning everything it just returns the first data member. id-func is designed that way because it's called with multiple arguments, and instead of acting on the arguments it wants to just return the first one, and I couldn't think of a better way to do that than to destructure it out.
;; Unfortunately I still can't! That means the code behind dir-vecs is...confusing at best and will stay that way.
;; matching-coordinates is quite simple - it just finds all the coordinates for which the piece matches the input pieces. It returns these as a vector like [1 1].
;; line-from is...a doozy. So, what's going on here is that - hold on, just, the formatting here is Not Cool.
line-from (fn [start-coords [f-r f-c]]
            (map #(vector % (get-in board %))
                 (take-while #(get-in board %)
                             (reductions (fn f [[r c] n]
                                           [(f-r r n) (f-c c n)])
                                         start-coords
                                         (repeat 1)))))
;; Eeeh not much better. Let's break this down. So, the reductions function basically gets the next square from the passed-in square, depending on f-r (function-row) and f-c (function-col), which are either +, -, or not-quite-identity. So, when you call (line-from [1 3] [+ +]) you're going to end up with [2 4] [3 5] ... - and you'd go forever, so it's a good thing reductions is lazy. The take-while cuts that off so that you don't run off the board. The map associates the coordinates with the value of the symbol in board. You'll end up with a sequence like ([[1 1] e] [[2 2] b] ... [[4 4] e]) or something, representing a line from - well you wouldn't get this, actually, since [1 1] would be empty and you can't make a move based in an empty square. But the point stands.
;;   placement is an unholy mess.
placement (fn [line]
            (let [[other [first-not-other & rest-not-other]] (split-with #(= other-piece (last %)) (rest line))]
              (if (and (> (count line) 2) 
                       (> (count other) 0) 
                       (= 'e (last first-not-other)))
                  {(first first-not-other) (into #{} (map #(first %) other))})))
;; Okay, so. First, let's address the split-with. The split-with is checking to see how many opposite pieces are on the line AFTER the friendly piece. So, you might have a line starting at [1 1] looking like [[1 1] 'w] [[2 2] 'b] [[3 3] 'b] [[4 4] 'w] [[5 5] 'e]; in this case the split-with would return (([[2 2] 'b] [[3 3] 'b]) ([[4 4] 'w] [[5 5] 'e])). Then, the destructuring puts that first sequence into other, the [[4 4] 'w] into first-not-other, and discards the rest so I'm not sure why I even bothered to bind it.
;; It checks that there's ample space to make a move (> (count line 2)), that there are opposite-colored pieces in the line (you can only make a move to land adjacent to an opposite-colored piece) and that there's an empty space at the end. If there is, it returns.
;;   You know what hold on
placement (fn [line]
            (let [[other-seq [first-not-other & _]] (split-with #(= other-piece (last %)) (rest line))]
              (if (and (> (count line) 2) 
                       (> (count other-seq) 0) 
                       (= 'e (last first-not-other)))
                  {(first first-not-other) (into #{} (map #(first %) other-seq))})))
;; Why do I even associate the coordinates with their values, though? We've got the board right there, yes?

line-from (fn [start-coords [f-r f-c]]
               (take-while #(get-in board %)
                           (reductions (fn f [[r c] n]
                                         [(f-r r n) (f-c c n)])
                                       start-coords
                                       (repeat 1))))
placement (fn [line]
            (let [[other-seq [first-not-other & _]] 
                    (split-with #(= other-piece (get-in board %)) (rest line))]
              (if (and (> (count line) 2)
                       (> (count other-seq) 0)
                       (= 'e (get-in board first-not-other)))
                  {first-not-other (into #{} other-seq)})))
;; aaaand it's sorely in need of work, but it's again getting late. Been a little stressful at work, I guess, and I don't quite have the fortitude to carry on, so I'll just call it here, atrocious as it is:
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
                       (take-while #(get-in board %)
                                   (reductions (fn f [[r c] n]
                                                 [(f-r r n) (f-c c n)])
                                               start-coords
                                               (repeat 1))))
        placement (fn [line]
                    (let [[other-seq [first-not-other & _]] 
                            (split-with #(= other-piece (get-in board %)) (rest line))]
                      (if (and (> (count line) 2)
                               (> (count other-seq) 0)
                               (= 'e (get-in board first-not-other)))
                          {first-not-other (into #{} other-seq)})))]
    (apply merge (filter map?
                         (for [coords matching-coordinates
                               dv dir-vecs]
                           (placement (line-from coords dv)))))))