;;;; 117 - For Science!
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/117-wip.clj
;; Original:
(fn for-science [coll]
  (letfn [(mark-next [coll]
            (->> (conj coll \#)
                 (reduce (fn [[out p c] n]
                           (if (and (contains? #{\space \C} c) (or (= p \F) (= n \F)))
                               [(conj out \N) \N n]
                               [(conj out c) c n]))
                          [[] nil nil])
                 ((comp rest first))))
          (swap-chars [coll]
            (map (fn [c]
                   (cond
                     (= c \N) \F
                     (= c \F) \#
                     :else c))
                 coll))
          (all-next [coll]
            (->> (into [] coll)
                 (map mark-next)
                 (apply map vector)
                 (map mark-next)
                 (map swap-chars)
                 (apply map vector)))]
    (loop [maze (map #(into [] (clojure.string/replace % #"M" "F")) coll)]
      (let [flat-maze (flatten maze)]
        (cond
          (not-any? #{\C} flat-maze) true
          (not-any? #{\F} flat-maze) false
          :else (recur (all-next maze)))))))
;;   This is another pathfinding problem! And, as I've stated before, my college
;; education has at least minimally prepared me for solving online problems by
;; making me at ease with various methods of pathfinding and basic AI.
;;   I didn't use any of that knowledge, though. I ended up figuring that a basic
;; flood-fill style search would be the simplest solution. So, what it basically
;; does it advances through the maze starting with the \M character, replacing
;; blanks with \#, until either it's run out of blank spaces into which it can
;; advance, or until it's replaced the \C. If it's run out of blanks without
;; replacing the \C character, there exists a path; otherwise, there's no such
;; path.
;;   The actual replacement is done by three functions, mark-next, swap-chars,
;; and all-next. Since it wouldn't be very elegant to be mucking around with
;; indices everywhere, which is how I would normally go about it, we have the
;; problem of how to replace vertically, given that the map is presented as an
;; array of strings. The solution is to simply rotate it by using (apply map
;; vector), and then rotate it back to produce the final result. This happens in
;; all-next.
;;   The actual replacement happens in mark-next and swap-chars. Mark-next steps
;; through the targeted string, reducing over the string with a 3-character
;; memory - it holds the current character, as well as the characters before and
;; after. If the current character is either empty, or \C (the end character),
;; and one of the adjacent characters is \F (for "fill"), it marks the character
;; with an \N. It doesn't mark it with an \F because doing so would actually
;; advance until it reached a \#, and that would actually prove fatal in some
;; cases, so we want to go one-by-one.
;;     (an example might be "#M        #"
;;                          "#  # ##   #"
;;                          "#     C   #") - this would fail!
;;   Once all the characters adjacent to \F characters are marked with \N, it
;; flips all of the \N makers over to \F and all the \F markers over to \#. This
;; repeats until it eventually either has turned all the traversable open spaces
;; to \#, or it finds \C.
;;   So, what can we clean up here?
;;   mark-next is kind of janky-looking, but logically it's solid. Maybe
;; reformatting will make it a little more appealing:
mark-next [coll]
  (->> (conj coll \#)
       (reduce (fn [[out p c] n]
                 (if (and (contains? #{\space \C} c) 
                          (or (= p \F) (= n \F)))
                     [(conj out \N) \N n]
                     [(conj out c) c n]))
                [[] nil nil])
       ((comp rest first))))
;;   The strange-looking (comp rest first) is there to keep it from adding extra
;; lines on each iteration. You need first so that it collects the proper array
;; out of the results returned from the reduction. It'll technically work fine
;; if you just use first - but it'll insert a ton of extra lines and that's not
;; cool.
;;   swap-chars is pretty short, but the cond could be a condp:
(swap-chars [coll]
  (map (fn [c]
         (condp = c
           \N \F
           \F \#
           c))
       coll))
;;   I do believe that's the first time I've ever used condp.
;;   all-next is...fine as it is, actually.
;;   There's not much else I can see that would be rewritten better at the moment
;; (though, I'm tired and...tired, so that might mean I'm less than perceptive
;; at the moment). Normally here I'd try to rewrite it utilizing a different
;; algorithm or something, but like I said, I'm dead tired and very frustrated
;; from work, so...it stands as-is.
;;   Though, it's one of my better first-time efforts, I think! There was nothing
;; that made me want to pull my hair out as soon as I looked at it. Progress!