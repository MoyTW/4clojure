;; Hey, crossword puzzles! I...don't really solve them.
;; Grandma did, though. It's probably more popular among the elderly - us younglings have our own entertainment, in the form of magical pocket-sized fun machines and huge televisions slaved to incomprehensabily complex computing machines.
;; I'm pretty sure I misspelled incomprehensably back there - or even here. Hold on.
;; incomprehensibly - in - comp - rehen - sibly.

;; Anyways.

;; What is our algorithm, here?
;; Well, how about "For each empty space, check if we can start the word here?" No, that wouldn't work, because you can have the first character prefilled. In fact, we have to check occupied spaces, because you can prefill the first letter!
;; So that won't work.
;; What must be true for the word to be placed on the board?
;; Must match sizes exactly, must be left-to-right

;; Okay, how about we pull out all the rows, columns?
;; Rows
(
(defn pull-horizontal [coll]
  (mapcat #(clojure.string/split (clojure.string/replace % #"\s" "") #"#") coll))
["c _ _ _" "d _ # e" "r y _ _"])
;; Columns
(
(defn pull-vertical [coll]
  (let [columns (apply map str (map #(clojure.string/replace % #"\s" "") coll))]
    (mapcat #(clojure.string/split % #"#") columns)))
["c _ _ _" "d _ # e" "r y _ _"])

;; Now, we should discount any row/column which isn't the same length as the word in question
(
(fn filter-possible-matches [word coll]
  (let [hori
          (mapcat 
            #(clojure.string/split (clojure.string/replace % #"\s" "") #"#")
            coll)
        vert 
          (mapcat 
            #(clojure.string/split % #"#") 
            (apply map str (map #(clojure.string/replace % #"\s" "") coll)))
        pmatches (filter #(= (count %) (count word)) (concat hori vert))]
    pmatches))
"the" ["c o n j" "_ _ y _" "r _ _ #"])

;; Uh, actually, this is pretty much it. Just feed a comparison function in.
(
(fn crossword-match [word space]
  (every? #(or (= (first %) (second %)) (= (second %) \_)) (apply map vector [word space])))
"the" "__e")

(
(fn filter-possible-matches [word coll]
  (let [filter-func
          (fn [space]
            (and (= (count space) (count word))
                 (every? #(or (= (first %) (second %)) (= (second %) \_)) 
                         (apply map vector [word space]))))
        hori
          (mapcat 
            #(clojure.string/split (clojure.string/replace % #"\s" "") #"#")
            coll)
        vert 
          (mapcat 
            #(clojure.string/split % #"#") 
            (apply map str (map #(clojure.string/replace % #"\s" "") coll)))
        pmatches (filter filter-func (concat hori vert))]
    (boolean (seq pmatches))))
"zoom" ["_ # _ _ e"])