;;; 70-73 - In Which There Are Three Very Short Problems And One Very Long One

;;; 70 - Word Sorting
;; Original:
#(sort-by clojure.string/lower-case (re-seq #"[A-z]+" %))
;; Two functions I haven't used before (and I don't think since) in this one - sort-by and re-seq. sort-by is fairly self-explanatory, I'd think - it calls the function on the members of the collection and sorts by them. It also takes a comparator which I think is a Java Thing, and I'm not sure exactly how to use it. It works just fine without, though.
;; I assume that if you knew Java you could use reify or something to whip up sweet awesome comparisons, but sadly...I do not.
;; Anyways, re-seq takes a sequence of regular expression matches, so (re-seq #"[A-z]+" %) is basically just grabbing all the words out of it into a sequence.
;; Anyways, it's a one-liner, or two-liner if you want to pull it out of the anonymous function, so I'll leave it without rewriting.

;;;; 71 - Rearranging Code: ->
;; Original:
last
;; This is a tutorial to the -> threading macro, which works on the front of a series of transformations and basically means you read them right-to-left rather than left-to-right.
;; I've never actually found this threading macro to be very helpful, I don't know why. On the other hand, I love the next one up, which is...

;;;; 72 - Rearranging Code: ->>
;; Original:
apply +
;; The ->> threading macro is actually pretty cool. It threads to the right, instead of the left, and it works on most of the transformations. The thing is that if you write a custom function, the ability to use the threading macros is totally dependant upon where you put your parameters, and some functions - like the strong functions - don't thread to the end, they thread to the front, which - I mean, I guess it's just personal taste.
;; I'm sure there's a perfectly good reason for the strings to thread left instead of right, but my poor feeble mind cannot conceive of it.

;;;; 73 - Analyze a Tic-Tac-Toe Board
;; Original:
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
            (if l-v l-v nil)))
        ; This handles horizontal/verticals
        (let [n (first rng)
              v-val (three-in-row (nth coll n))
              h-val (three-in-row (reduce #(conj %1 (nth %2 n)) [] coll))]
          (if v-val
            v-val
            (if h-val
              h-val
              (recur (rest rng)))))))))
;;   For the record, that comment? In the original code.
;; So it's pretty confusing (and I wrote it, too) but after a couple of minutes of confusion, here's what's going on. First, the three-in-row function takes a collection and returns :o if all three are :o, :x if all three are :x, and nil if neither of the above are true.
;; The rest of the function runs through row/column sets 0 through 2, applying three-in-row to them and returning if three-in-row isn't nil. If it runs through all the row/column pairs, it switches to checking the diagonals, and if the diagonals are nil, it terminates with a return value of nil.
;; It's, uh, it's pretty gosh-darn horrifying looking at it right now, so let's clean it up a bit:
(fn check-lines [coll]
  (loop [rng (take 3 (range))]
    (let [three-in-row (fn three-in-row [coll]
                         (if (and (= (coll 0) (coll 1) (coll 2))
                                  (not= (coll 0) :e))
                             (coll 0)
                             nil))]
      ; This handles diagonals
      (if (= '() rng)
        (let [r-v (three-in-row [((coll 0) 0) ((coll 1) 1) ((coll 2) 2)])
              l-v (three-in-row [((coll 0) 2) ((coll 1) 1) ((coll 2) 0)])]
          (if r-v
            r-v
            (if l-v l-v nil)))
        ; This handles horizontal/verticals
        (let [n (first rng)
              v-val (three-in-row (nth coll n))
              h-val (three-in-row (reduce #(conj %1 (nth %2 n)) [] coll))]
          (if v-val
            v-val
            (if h-val
              h-val
              (recur (rest rng)))))))))
;; Eeeh, you know what, screw cleaning it up, the whole thing's terribly "Run down all possibilities in a hamfisted manner" and we may as well ditch it. Let's find a more compact way to express this.
;; How about we do something like, "Get all the possible vectors, and then loop/recur over them, looking for not nils"? I mean, that's *kind* of what's happening up there, but, well, the idea is we can compact the code greatly. Here, watch - let's build the vectors:
(
(fn build-vectors [coll]
  (let [rng (take 3 (range))
        pull-hori (fn [n] (reduce #(conj %1 (%2 n)) [] coll))
        straight-vectors (mapcat #(vector (coll %) (pull-hori %)) rng)]
    (conj straight-vectors
          [((coll 0) 0) ((coll 1) 1) ((coll 2) 2)]
          [((coll 0) 2) ((coll 1) 1) ((coll 2) 0)])))
[[:x :e :e] [:o :x :e] [:o :e :x]])
;; Then we can do this in a much more compact fashion, by putting this helper-function in as such:
(fn check-lines [coll]
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
;; Well, that's still pretty darn ugly. Especially because if the way the function definitions are trying to escape the right side of the screen...I really, really, really wish that 4Clojure would let me def functions. On the other hand we're down to 19 lines and there's no convoluted if chain! So I guess it's a tradeoff?
;; Also, hard-coding in the diagonals? That's inelegant as all heck. On the other hand, it is the simplest thing that could possibly work. I'm conflicted about this.
;; We could actually simplify it further by mapping over the vectors, but the issue with that is then you're always checking all of the possibilities. On the other hand, the number of possibilities is always eight, with no chance of it ever increasing, so it's not like it's an unbearable computational overhead...
;;   I wonder how other people did it? Let's go look at The Internets!

;; From https://github.com/ardumont/org/blob/master/clojure/4clojure-73-analyze-a-tic-tac-toe-board.org
(fn ttt [b]
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
    (cond (w b :x) :x
          (w b :o) :o
          :else nil)))
;; Uh, so, this is pretty crazy. What it's doing is defining a function w, which takes a tic-tac-toe board and destructures it to a b c d e f g h i, and then explicitly checks each of the eight different possible combinations. That's...that's pretty ingenous, actually! It's ugly as all hell but it's very ingenious. Also, it's really short, so it's got that going for it!

;; From https://github.com/reillywatson/4clojure/blob/master/73.clj
(fn an [board] (let [vic (first (first (filter #(= (count %) 1) (map distinct
  (concat (list
    (map first board)
    (map second board)
    (map #(nth % 2) board)
    (map-indexed #(nth %2 %) board)
    (map-indexed #(nth %2 (- 2 %)) board)) board)))))] (if (= vic :e) nil vic)))
;; I have no idea what's going on here. Okay, let's reformat this to be less insane:
(defn an [board] 
  (let [vic (ffirst 
              (filter #(= (count %) 1)
                      (map distinct
                           (concat (list (map first board)
                                         (map second board)
                                         (map #(nth % 2) board)
                                         (map-indexed #(nth %2 %) board)
                                         (map-indexed #(nth %2 (- 2 %)) board))
                                   board))))]
    (if (= vic :e) nil vic)))
;; Well, this is really something else. Okay, what it appears to be doing is filtering for single-element collections from a map of distinct elements from...hmm. So, what's this concat doing? It's creating a list of the verticals and diagonals, and then adding the board to create a list of all the different three-element combinations.
;; So, the map distinct then cuts down on the three-element rows which are identical - if the row is (:x :x :x), for example, it will return (:x). It then filters by the count, and...huh. Okay, I get what's going on here! That's a heck of a convoluted way to go about it but hey, it's three lines shorter than mine! The ffirst is sketchy as heck, though; is there anything other than coincidence which prevents the ffirst from grabbing a :e instead of an :x? For example, try the following:
(an [[:e :o :x]
     [:e :o :x]
     [:e :e :x]])
;; And it comes up nil! So, no, there's nothing but concidence. You could ameliorate it by doing something like checking for the presence of either :x or :o in the returned data structure, instead of taking the (ffirst) element.
;; I think my favorite one is the destructuring method, despite the fact that it literally enumerates every single valid combination, just because that's pretty darn clever, actually.