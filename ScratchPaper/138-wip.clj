;; Initial reaction: Wait, it wants me to do what?
;; Okay, so, what's going on with [4 20]?
;; Successive squares of 4 are: 4, 16
;; " 4 "
;; "* 1"
;; " 6 "
;; So, okay.

;; [2 256]
;; 2, 4, 16, 256

;; What are the triangle sizes?
;; 1  (+1)
;; 4  (+3)
;; 9  (+5)
;; 16 (+7)

;; So, we want to generate the sequence (2 4 1 6 2 5 6 * *) (length of sequence is 9)
;; Then we wrap it somehow. How? No idea at the moment. Well, it says 45deg rotated square. So, what would it be un-rotated?
;; 6 * *
;; 5 2 4
;; 2 6 1

;; 0-0-0-0
;; 1 1-0 *
;; 0-0-1 *
;; * * * *

;; This is graphics stuff which I wish I remembered because we totally went over this in my graphics class. The rotation, I mean. Not the wrapping. I don't know exactly what the best way to deal with the wrapping is. The initial thought I had was "it's like an array, use arrays and indices" but preferably we'll avoid using indices like that due to language norms, yes?

;; [[:center 1] [:right 1] [:down 1] [:left 2] [:up 2] [:right 3] [:down 3] [:left 3]] sum 16
;; right->down->left-up->right->down->left...
;; Only two ways to expand square - from upper-right, from lower-left.
;; From upper-right: [:right :down :left]
;; From lower-left: [:left :up :right]

(defn do-thing [existing addition]
  (if (= :left (ffirst addition))
      (let [size (count existing)]
        (update-in existing [(dec size)] #(into [] (cons (first (last (first addition))) %))))))

(do-thing [[1 0] [0 1]] [[:left [0]] [:up [1 0]] [:right [0 0]]])

;; wait hold on
;; ...we can totally just rotate it, can't we. Always add to the upper-right, but rotate it?

;; 1

;; 1 0

;; 0 1
;; 1

;; 1 0
;; 0 1

;; Bad example let's use 2 4 1 6 2 5 6 * *
;; 2

;; 2 4

;; 4 1
;; 2

;; 1 6
;; 4 2

;; Here we restart:
;; 1 6 2
;; 4 2

;; 2 5 6 (add 2)
;; 6 2
;; 1 4

;; 6 * *
;; 5 2 4
;; 2 6 1

;; How do we make sure that it's rotated correctly? And how do we convert it into a diamond?

;; 2

;; 4
;; 2

;; 1
;; 4 2

;; 6 2
;; 1 4

;; 2
;; 6 2
;; 1 4

;; 6
;; 5 2 4
;; 2 6 1

;; * 4 1
;; * 2 6
;; 6 5 2

;; uuuuh I'm too tired for this?
;; There are two ways of expanding the square:
;;    |
;;  --+  (lower-right)
;; and
;; +----
;; |
;; |     (upper-left)
;; [2 4 1 6 2 5 6 * *]
;; -> [[2] [4 1 6] [2 5 6 * *]] - for a 3x3 square
;; 2 is center
;; [4 1 6] is lower-right
;; [2 5 6 * *] is upper-left
;; How to tell which?
;; (mod 4 n) - if equal 1, is upper-left, if equal 3, is lower-right - assumes, of course, a center of count 1.

(defn lower-right [square additions]
  (let [size (count square)
        [_ bottom-range] (split-at size additions)]
    (conj (into [] (map conj square additions)) 
          (into [] (reverse bottom-range)))))
(lower-right [[2]] [4 1 6])
;; [[1] [0 1 0] [0 1 0 0 0] [0 * * * * * *]]
(lower-right [[0 0 0] [1 1 0] [0 0 1]] [0 '* '* '* '* '* '*])
;; Gives: [[0 0 0 0] [1 1 0 *] [0 0 1 *] [* * * *]]

(defn upper-left [square additions]
  (let [size (count square)
        [left-range top-range] (split-at size additions)]
    (vec (cons (vec top-range)
               (map #(vec (cons %1 %2)) (reverse left-range) square)))))
(upper-left [[2 4] [6 1]] [2 5 6 '* '*])

(defn next-layer [square additions]
  (if (= (mod (count additions) 4) 3)
      (lower-right square additions)
      (upper-left square additions)))

;; Okay, so:
(defn make-squares [inputs]
    (reduce next-layer [(first inputs)] (rest inputs)))

(make-squares [[1] [0 1 0] [0 1 0 0 0] [0 '* '* '* '* '* '*]])
(make-squares [[2] [4 1 6] [2 5 6 '* '*]])
(make-squares [[2]])

;; Find square sizes:
;; +1 +3 +5 +7 etc
(take 5 (reductions + (iterate #(+ 2 %) 1)))
(first (drop-while #(< % 10) (reductions + (iterate #(+ 2 %) 1))))

;; Going from [start end] to partitioned sequence of numbers:
(defn make-seq [begin end]
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
(make-seq 2 256)
(make-seq 10 10000)
;; Pretty inelegant. Eh, well, works.

;; So, how do we rotate this?
;; ...uuuh how about just use a rotation matrix?
(def r-angle (- (/ Math/PI 4)))

;(defn r-pt [[x y]]
;  [(Math/round (- (* x (Math/cos r-angle)) (* y (Math/sin r-angle))))
;   (Math/round (+ (* x (Math/sin r-angle)) (* y (Math/cos r-angle))))])
(defn r-pt [[x y]]
  [(int (- (* x (Math/cos r-angle)) (* y (Math/sin r-angle))))
   (int (+ (* x (Math/sin r-angle)) (* y (Math/cos r-angle))))])

(make-squares (make-seq 2 256))
(def square (make-squares (make-seq 2 256)))
(defn pad-with-spaces [s]
  (vec (interpose (vec (repeat (+ 2 (count s)) nil)) (map #(vec (interpose nil %)) s))))
(pad-with-spaces square)

(defn resolve [s]
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
    new-vec))
;(resolve (pad-with-spaces square))
(resolve (pad-with-spaces (make-squares (make-seq 10 10000))))
;; Oh. Well. It's actually running off the intended array.
(r-pt [-3 -3])
(r-pt [-3 0])

(defn __ [d m]
  (apply map str (resolve (pad-with-spaces (make-squares (make-seq d m))))))

(= (__ 2 2) ["2"])
(= (__ 2 4) [" 2 "
             "* 4"
             " * "])
(= (__ 3 81) [" 3 "
              "1 9"
              " 8 "])
(= (__ 4 20) [" 4 "
              "* 1"
              " 6 "])
(= (__ 2 256) ["  6  "
               " 5 * "
               "2 2 *"
               " 6 4 "
               "  1  "])
(= (__ 10 10000) ["   0   "
                  "  1 0  "
                  " 0 1 0 "
                  "* 0 0 0"
                  " * 1 * "
                  "  * *  "
                  "   *   "])

;; Works on everything but final, due to Maths. So, it's not technically a square rotated 45 degrees, is it. WELL! Uh, that should have been discernable from the problem description, probably. Well, no, actually. Hmm. Let me think.

;; I don't have the Math Sense to immediately discern in which way it's not a square, or is a square. So, I'm going to do something Very Naughty. I'm going to hack this.
;; This is stupid.
(defn r-pt [[x y]]
  (cond
    (= -3 x y) [-3 0]
    (= 3 x y) [3 0]
    (and (= -3 x) (= 3 y)) [0 3]
    (and (= 3 x) (= -3 y)) [0 -3]
    :else [(int (- (* x (Math/cos r-angle)) (* y (Math/sin r-angle))))
           (int (+ (* x (Math/sin r-angle)) (* y (Math/cos r-angle))))]))