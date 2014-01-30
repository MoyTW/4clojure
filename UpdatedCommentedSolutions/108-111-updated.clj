;;;; 108 - Lazy Searching
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/108-wip.clj
;; Original:
(fn lazy-search [& seqs]
  (if (= 1 (count seqs)) (ffirst seqs)
    (loop [seqs seqs]
      (let [firstvals (map first seqs)
            low-val (apply min firstvals)]
        (if (apply = firstvals) low-val
          (recur (map #(if (= low-val (first %)) (rest %) %) seqs)))))))
;;   The very first thing to do here is align the arguments to if properly. At
;; some point I thought that formatting my ifs like (if pred result \n else) was
;; a great idea, but it really isn't.
(fn lazy-search [& seqs]
  (if (= 1 (count seqs))
      (ffirst seqs)
      (loop [seqs seqs]
        (let [firstvals (map first seqs)
              low-val (apply min firstvals)]
          (if (apply = firstvals) 
              low-val
              (recur (map #(if (= low-val (first %)) (rest %) %) seqs)))))))
;;   Anyways! Now that that's over with. The way that this algorithm solves the
;; problem is by picking the lowest value out of all the different sequences. If
;; that lowest value is equal to the first member of every sequence, then you've
;; found what you're looking for! If it's not, simply remove the lowest value
;; from the front of the sequence containing it, and call it again.
;;   This, of course, assumes that it's solvable. If it's not, you run into
;; Problems (but you could modify it to handle that easily enough). Since it's
;; 4Clojure, we just assume it's solvable and all.
;;   Other minor, non-algorithm changing code modifications! We don't actually
;; need that loop; we can recur just as well to the function itself. Also that
;; anonymous function is a little compressed. Maybe decompress it a bit?
(fn lazy-search [& seqs]
  (if (= 1 (count seqs))
      (ffirst seqs)
      (let [firstvals (map first seqs)
            low-val (apply min firstvals)]
        (if (apply = firstvals) 
            low-val
            (recur (map #(if (= low-val (first %)) 
                             (rest %) 
                             %)
                        seqs))))))
;;   That's kind of ugly, but it's probably more readable. Uglier but more
;; readable; I don't know if I'd take it or not! (Obviously, more readable is
;; the superior choice, but ugly...)
;;   What other algorithms could be used? Let's check with the almighty Internets
;; and mine the collected wisdom of the masses.
;;   Okay, back from the Internets, and you know what? Like, the entire first
;; page of solutions to 108 were variations of "Recursively remove the
;; lowest-value elements from the collection of sequences until everything lines
;; up right" dressed up with slightly different implementations. It's good to
;; know you're average, isn't it?

;;;; 109 - No Such Problem

;;;; 110 - Sequence of pronunciations
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/110-wip.clj
;; Original:
(fn weird [coll]
  (letfn [(step [coll]
           (mapcat #(list (count %) (first %))
                   (partition-by (fn [x] x) coll)))]
    (rest (iterate step coll))))
;;   The algorithm for this one kind of confused me until I sat down and wrote it
;; out by hand.
;;   Well, it still confused me the first time I wrote it out by hand, but I got
;; through that and eventually got it. What this algorithm does is basically
;; find repeated values, and then if there are repeated values, it counts them
;; and inserts the count into the list at the appropriate time while removing
;; the duplicates. Specifically, it partitions by identity (and apparently I
;; didn't know about the identity since it's a custom function returning the
;; input). Then it mapcats over the partitioned values, returning [count value]
;; (which is then flattened by mapcat).
;; It's pretty darn simple, actually, except for that annoying (fn [x] x):
(fn weird [coll]
  (letfn [(step [coll]
            (mapcat #(list (count %) (first %))
                    (partition-by (fn [x] x) coll)))]
    (rest (iterate step coll))))

;;;; 111 - Crossword Puzzle
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/111-wip.clj
;; Original:
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
;;   I've never actually done many crossword puzzles. Actually, I think I've
;; never done *any* crossword puzzles, but Grandma used to do them.
;;   Initially, this looked very frightening, but it turns it it...really wasn't.
;; So, the algorithm I use here is basically this:
;;     * What are all the horizonontal possible matches?
;;     * What are all the vertical possible matches?
;;     * Of all the matches, are there any which:
;;       * Have the appropriate number of spaces AND
;;       * Each space either matches the word, or is blank
;;   So, there are three main functions, and then it combines them to find all
;; possible matches and, if there are any, returns true. It is, admittedly,
;; pretty ugly.
(fn filter-possible-matches [word coll]
  (let [matches? (fn [space]
                   (and (= (count space) (count word))
                        (every? #(or (= (first %) (second %))
                                     (= (second %) \_))
                                (apply map vector [word space]))))
        hori (mapcat #(-> (clojure.string/replace % #"\s" "")
                          (clojure.string/split #"#"))
                     coll)
        vert (mapcat #(clojure.string/split % #"#")
                     (->> coll
                          (map #(remove #{\space} %))
                          (apply map str)))
        all-matches (filter matches? (concat hori vert))]
    (boolean (seq all-matches))))
;;   Well, it doesn't exactly make it amazingly pretty but I think that's a
;; little more readable now. I'm not sure if there's a better way to get the
;; matches? function to look; the and every or thing is a little eeeeh;
;; especially that anonymous function! The only thing that's stopping me from
;; putting that into a proper function is, well, that'd lead to even more bloat
;; than there already is (you'd need at least a line for (fn [w s] ...))...
;;   As for the algorithm itself, though, I'm behind it! I do like it quite a
;; lot. Probably there's a way to optimize it but I don't think it's actually
;; slow - it basically is linear along the input size of the puzzle - so that's
;; not bad at all.