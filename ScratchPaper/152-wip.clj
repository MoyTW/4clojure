;; Hmmmmm. Okay.
;; This problem would be actually almost trivial to implement using an index-based algorithm. Could hammer this out in Python right quick, I think.
;; If you're not using indices, uh. Hmm. How do we get cols?
;; oh right #(apply map vector %)

;; Okay, I think I got this.
;; First, we generate all the alignments, with empty spaces padded with nil.
;; Then, we take the vertical and horizontal counts.
;; The largest square is (min v h), and smallest is 2.
;; For each size, traverse the 2-d array, taking a size-length slice.
;; Check the slice for latin squares.
;; If there is a latin square, shove it into the set of squares.


;; Okay, getting size=2 candidates, width=5, height=3:
; 1 2 3 n n
; 2 3 1 2 1
; 3 1 2 n n
;; Range to take from width: (range (- (inc width) size)) = [0 1 2 3]
;; Range to take from height: (range (- (inc height) size)) = [0 1]
;; To get the rows: (take size (drop from-height array))
;; aaand map (take size (drop-from-width array))
;; So let's see how this works:

(def a
  [[1 2 3 nil nil]
   [2 3 1 2 1]
   [3 1 2 nil nil]])
(defn candidates [size array]
  (let [height (count array)
        height-range (range (- (inc height) size))
        width (count (first array))
        width-range (range (- (inc width) size))]
    (for [row height-range
          col width-range
          :let [segment (map #(take size (drop col %))
                             (take size (drop row array)))]
          :when (not-any? #(= nil %) (flatten segment))]
      segment)))
(candidates 2 a)
(candidates 3 a)
;; Seems to work.

;; Okay, now, detecting latin squares:
(def s1 [[1 2][2 3]])
(def s2 [[2 1][1 2]])
(def size 2)
;; Are there n distinct members for the nxn array?
(defn uniques? [array] (= (count array) (count (distinct (flatten array)))))
(uniques? s1)
(uniques? s2)
;; aaand:
(defn latin? [square]
  (if (uniques? square)
      (and (every? uniques? square)
           (every? uniques? (apply map vector square)))))
(latin? s1)
(latin? s2)

(def s3
  [['A 'B 'C]
   ['B 'C 'A]
   ['C 'A 'B]])
(def s4
  [['A 'B 'C]
   ['B 'C 'A]
   ['C 'A 'C]])
(def s5
  [['A 'B 'C]
   ['B 'D 'A]
   ['C 'A 'B]])
(latin? s3)
(latin? s4)
(latin? s5)

;; Now, gen alignments:
;; hmmmm
; [[1]       <- shifts 3 times = 4
;  [1 2 1 2]
;  [2 1 2 1]
;  [1 2 1 2]
;  []       ]
;; How many alignments? 4.
; [[1 2 3]     <- shifts two times = 3
;  [2 3 1 2 1]
;  [3 1 2]]    <- shifts two times = 3
; This has 9.
;; Hey, I wish I remember my Maths better.
;; Anyways. Find the max size. Then, we find difference per each less, and multiply.
(def se [[1 2 3][2 3 1 2 1][3 1 2]])
(let [[max & less] (sort > (map count se))]
  (map #(- max %) less))
(let [x (map count se)
      m (apply max x)]
  (map #(- m %) x))
;; Okay, now, how to do shifts:
(defn gen-shifts [width coll]
  (let [pad (- width (count coll))
        do-shift #(conj (vec (rest %)) nil)]
    (if (zero? pad)
        [coll]
        (take (inc pad) 
              (iterate do-shift 
                       (vec (apply conj (seq coll) 
                                        (repeat pad nil))))))))

(map #(gen-shifts 5 %) se)
(defn merge-rows [out in]
  (for [o out
        i in]
    (conj o i)))

(def ss (map #(gen-shifts 5 %) se))
(merge-rows (merge-rows (merge-rows [[]] (first ss)) (second ss)) (last ss))
(reduce merge-rows [[]] ss)

(map latin? (mapcat #(candidates 2 %) (reduce merge-rows [[]] ss)))
(filter latin? (mapcat #(candidates 2 %) (reduce merge-rows [[]] ss)))
; (((2 1) (1 2)) ((2 1) (1 2)) ((2 1) (1 2)))
(filter latin? (mapcat #(candidates 3 %) (reduce merge-rows [[]] ss)))
; (((1 2 3) (2 3 1) (3 1 2)))
;; I think we've got all the parts needed, now.
(defn square-latin [n coll]
  (filter latin? (mapcat #(candidates n %) coll)))

(defn get-latin [coll]
  (let [m (first (sort > (map count coll)))
        array (reduce merge-rows [[]] (map #(gen-shifts m %) coll))
        sizes (range 2 (inc (min (count coll) (count (first coll)))))]
    (mapcat #(square-latin % array) sizes)))

(distinct (get-latin se))
(into {} (for [[k v] (group-by count (distinct (get-latin se)))] [k (count v)]))

(defn __ [coll]
  (let [squares (distinct (get-latin coll))]
    (into {}
          (for [[k v] (group-by count squares)]
            [k (count v)]))))

(= (__ [[1]
        [1 2 1 2]
        [2 1 2 1]
        [1 2 1 2]
        []       ])
   {2 2}) ; false
(def sz
  [[1]
   [1 2 1 2]
   [2 1 2 1]
   [1 2 1 2]
   []       ]) ; this fills with nils, explodes
;; Bah! Add a special case?

(defn gen-shifts [width coll]
  (let [pad (- width (count coll))
        do-shift #(conj (vec (rest %)) nil)]
    (cond
      (zero? pad) [coll]
      (empty? coll) [(vec (repeat pad nil))]
      :else
        (take (inc pad) 
              (iterate do-shift 
                       (vec (apply conj (seq coll) 
                                        (repeat pad nil))))))))
(reduce merge-rows [[]] (map #(gen-shifts 4 %) sz))
;; hmm, why is this not working
  
(= (__ '[[B D A C B]
         [D A B C A]
         [A B C A B]
         [B C A B C]
         [A D B C A]])
   {3 3}) ; true
;; Everything but the one with nils works...what's the issue?
;; Well, it's the only one which has a height greater than width. So, that's a hint.
(mapcat #(candidates 2 %) (reduce merge-rows [[]] (map #(gen-shifts 4 %) sz)))
;; hmm?
(get-latin (reduce merge-rows [[]] (map #(gen-shifts 4 %) sz)))

(defn square-latin [n coll]
  (filter latin? (mapcat #(candidates n %) coll)))

(defn get-latin [coll]
  (let [m (first (sort > (map count coll)))
        array (reduce merge-rows [[]] (map #(gen-shifts m %) coll))
        sizes (range 2 (inc (min (count coll) (count (first coll)))))]
    (mapcat #(square-latin % array) sizes)))
    
(first (sort > (map count sz))) ;4
(reduce merge-rows [[]] (map #(gen-shifts 4 %) sz))

;([[nil nil nil 1] 
;  [1 2 1 2]
;  [2 1 2 1]
;  [1 2 1 2]
;  [nil nil nil nil]]
; [[nil nil 1 nil]
;  [1 2 1 2]
;  [2 1 2 1]
;  [1 2 1 2]
;  [nil nil nil nil]] 
; [[nil 1 nil nil]
;  [1 2 1 2]
;  [2 1 2 1]
;  [1 2 1 2] 
;  [nil nil nil nil]] 
; [[1 nil nil nil]
;  [1 2 1 2]
;  [2 1 2 1]
;  [1 2 1 2] 
;  [nil nil nil nil]])
;; Hmm.
(range 2 (inc (min (count sz) (count (first sz))))) ; is ()
;; I think I found the error.
(defn get-latin [coll]
  (let [m (first (sort > (map count coll)))
        array (reduce merge-rows [[]] (map #(gen-shifts m %) coll))
        sizes (range 2 (inc (min (count coll) m)))]
    (mapcat #(square-latin % array) sizes)))
(__ sz)
;; Huzzah!