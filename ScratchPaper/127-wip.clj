;; Well, well, well. So it's a kind of "Find the biggest shape you can fit in here" or somesuch.
;; hhhhhhmmmmmmmmmm
;; I have little idea of where to start for that!

;; Well, when all clever solutions fail, try out brute force, right?
;; In this case we could go something like:
;;   For each true node, extrapolate the largest possible triangle which can be formed with that node as the base. Store the largest. Repeat for each node, and that's pretty much it, right?
;; So, as an example, take the following:
; 10001
; 10110
; 00110
; 01110
; 10110
;; Let's say we're analyzing the very center node. We set it as the tip, and then check for the eight different possible triangles formed, as such:
;; N:
; 10001
;  011
;   1  
;
;
;; NE:
;   0
;   11
;   110
; 
; 
;; E:
;     1
;    10
;   110
;    10
;     0
;; SE:
; 
; 
;   110
;   11
;   1
;; and so on and so on.
;; There's probably some really clever way to harnass the bitmap to be useful but I'm just going to turn it into a binary 2D array:

(def a [17 22 6 14 22])

(defn ints-to-array [bitmap]
  (map #(Integer/toBinaryString %) bitmap))
(ints-to-array a)
;; Oh dear, it's removing leading zeroes.
(defn int-to-array [i c]
  (clojure.string/replace (->> (Integer/toBinaryString i)
                               (format (str \% c \s)))
                          \space
                          \0))
(defn ints-to-array [bitmap]
  (map #(int-to-array % (count bitmap)) bitmap))
(ints-to-array a)
;; That's...convoluted, there's probably a better way to do it. Maybe cast it back to an integer before you throw it into format and then tell it to add in leading zeroes or something.
;; anyways it ends up in strings hold on
(defn int-to-array [i c]
  (->> (Integer/toBinaryString i)
       (Integer/parseInt)
       (format (str \% \0 c \d))
       (map #(= \1 %))
       (into [])))
(defn ints-to-array [bitmap]
  (into [] (map #(int-to-array % (count bitmap)) bitmap)))
(ints-to-array a)

;; Okay, so. How do we get from here to there? Well, if I were doing an imperative-style approach with all the array indices and things, I'd just go "increment forever!" and step upwards count by count, checking to see if all the data members are 1s.
;; What would be a better way to do it?
;; eh screw it indices it is
;; UPWARDS, base [3 3]
;; Area = 4 -> [3 3] [2 2] [2 3] [2 4]
;; Area = 9 -> [3 3] [2 2] [2 3] [2 4] [1 1] [1 2] [1 3] [1 4] [1 5]
(defn next-coords [x-min x-max y]
  (for [x (range (dec x-min) (+ 2 x-max))] [x (dec y)]))
  
(defn next-coords [c r-min r-max]
  (for [r (range (dec r-min) (+ 2 r-max))] [(dec c) r]))
(next-coords 3 3 3)
(defn next-coords [coords]
  (let [c (ffirst coords)
        r-min (last (first coords))
        r-max (last (last coords))]
    (for [r (range (dec r-min) (+ 2 r-max))]
      [(dec c) r])))
(next-coords '([3 3]))
(next-coords (next-coords '([3 3])))
;; Okay, to find the largest triangle:
(defn largest-vertical [coords mine n]
  (if (every? true? (map #(get-in mine %) coords))
      (recur (next-coords coords) mine (+ n (count coords)))
      n))
(largest-vertical '([3 3]) (ints-to-array a) 0)
(largest-vertical '([4 2]) (ints-to-array a) 0)

(def first-array [15 15 15 15 15])
(def second-array [1 3 7 15 31])
(largest-vertical '([4 2]) (ints-to-array first-array) 0)
(largest-vertical '([4 2]) (ints-to-array second-array) 0)
(def square-true [31 31 31 31 31])
(largest-vertical '([4 2]) (ints-to-array square-true) 0)
;; I think largest-vertical works (it should be decompressed into a loop/recur though)

;; Okay, how about diagonal? Say, NE.
;; [2 2]
;; [1 2] [2 3]
;; [0 2] [1 3] [2 4]
;; oh, you can map [-1 r] [c +1]...
(defn next-ne [coords]
  (into #{} (mapcat (fn [[r c]] [[(dec r) c] [r (inc c)]]) coords)))

(next-ne [[2 2]])
(next-ne (next-ne [[2 2]]))

(def fourth-array [7 3])

(defn largest-diagonal [coords mine n]
  (if (every? true? (map #(get-in mine %) coords))
      (recur (next-ne coords) mine (+ n (count coords)))
      n))
(largest-diagonal '([1 1]) (ints-to-array fourth-array) 0)

;; WAIT my ints-to-array is fatally flawed
;; We actually want to pad it to the length of the largest string.
;(defn int-to-array [i c]
  ;(->> (Integer/toBinaryString i)
;       (Integer/parseInt)
       ;(format (str \% \0 c \d))
       ;(map #(= \1 %))
;       (into [])))
;(defn ints-to-array [bitmap]
  ;(into [] (map #(int-to-array % (count bitmap)) bitmap)))
;(ints-to-array a)
(defn int-to-array [i c]
  (->> (Integer/parseInt i)
       (format (str \% \0 c \d))
       (map #(= \1 %))
       (into [])))
(defn ints-to-array [bitmap]
  (let [strs (map #(Integer/toBinaryString %) bitmap)
        pad-to (apply max (map count strs))]
    (into [] (map #(int-to-array % pad-to) strs))))
(ints-to-array a)
(ints-to-array [7 3])
;; Ah, that's better.

(largest-diagonal '([1 1]) (ints-to-array fourth-array) 0)
;; okay so
;; How do we get full coverage? EITHER add directional to the next steps, or basically rotate the maze and run them four times. Eh, that's pretty easy, just transpose and reverse.
(defn gen-mine-permutations [mine]
  [mine
   (into [] (reverse mine))
   (into [] (apply map vector mine))
   (into [] (reverse (apply map vector mine)))])
(gen-mine-permutations [[:a :b :c] [:d :e :f] [:g :h :i]])
;; this is kind of hilariously brute-force inelegant; beautiful in its own way, I think...

(defn find-max [coord mine]
  (max (largest-diagonal [coord] mine 0)
       (largest-vertical [coord] mine 0)))
(find-max [1 1] (ints-to-array [7 3]))

(defn jesus-wept [bitmap]
  (let [mine (ints-to-array bitmap)
        all-coords (for [r (range (count mine)) c (range (count (first mine)))] [r c])
        all-mines (gen-mine-permutations mine)]
    (apply max 
           (for [c all-coords
                 m all-mines]
             (find-max c m)))))
    
(jesus-wept [7 3])

(and
  (= 10 (jesus-wept [15 15 15 15 15]))
  (= 15 (jesus-wept [1 3 7 15 31])) ;NOPE
  (= 3 (jesus-wept [3 3]))
  (= 4 (jesus-wept [7 3]))
  (= 6 (jesus-wept [17 22 6 14 22]))
  (= 9 (jesus-wept [18 7 14 14 6 3]))
  (= nil (jesus-wept [21 10 21 10]))
  (= nil (jesus-wept [0 31 0 31 0])) ;ALSO NOPE
)
;; Okay it's like, 9:40. So I'll come back to this later when I'm not tired and stuff.

;; hahaha, that's not how you rotate matrices
;; this is how you rotate matrices
(defn gen-mine-permutations [mine]
  [mine
   (into [] (map (comp vec reverse) (apply map vector mine)))
   (into [] (reverse (map (comp vec reverse) mine)))
   (into [] (reverse (apply map vector mine)))])

;; There we go.