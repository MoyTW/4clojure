;;;; 94 - Game of Life
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/94-wip.clj
;; Original:
(fn life [board]
  (let [y-max (count board)
        x-max (count (first board))
        adj-coords
          (fn [[x y]]
            (for [x-n [(dec x) x (inc x)]
                  y-n [(dec y) y (inc y)]
                  :when (not= [x-n y-n] [x y])]
            [x-n y-n]))
        count-adj
          (fn [[x y] cells]
             (let [adj (adj-coords [x y])]
               (reduce #(if (contains? cells %2) (inc %1) %1) 0 adj)))
        lives?
          (fn [[x y] cells]
            (let [num-live (count-adj [x y] cells)]
              (if (contains? cells [x y])
                (boolean (#{2 3} num-live))
                (boolean (#{3} num-live)))))
        step
          (fn [cells]
            (for [x-n (take x-max (range))
                  y-n (take y-max (range))
                  :when (lives? [x-n y-n] cells)]
              [x-n y-n]))
        collapse-str
          (fn [s y]
            (->> s
                 (map #(if (= \# %2) [%1 y] nil) (take (count s) (range)))
                 (filter #(not= % nil))
                 (into #{})))
        collapse-vec
          (fn [v]
            (->> v
                 (map #(vector %1 %2) (reverse (take (count v) (range))))
                 (reduce #(concat %1 (collapse-str (last %2) (first %2))) [])
                 (into #{})))
        explode-set
          (fn [s]
            (let [spaces-list (into [] (take x-max (repeat \space)))
                  y-range (take y-max (range))
                  live-map
                    (reduce (fn map-set-to-rows [mp st]
                              (assoc mp
                                (last st)
                                (conj (get mp (last st)) (first st))))
                            (zipmap y-range (take y-max (repeat [])))
                            s)
                  explode-str
                    (fn [s-e]
                      (->> s-e
                           (reduce #(assoc %1 %2 \#) spaces-list)
                           (apply str)))]
              (->> y-range
                   (into [])
                   (map #(explode-str (get live-map %)))
                   (reverse))))]
    (explode-set (step (collapse-vec board)))))
;;   So, this is the old classic, the Game of Life. There are two sections to the
;; problem, actually. First, we need to convert to and from the array-of-strings
;; representation specified in the problem, and second, we need to actually do
;; the transformation. So, the plan to solve the game was basically to chain
;; three functions, as follows:
;;   * Convert to internal representation
;;   * Transform the board
;;   * Convert to array of strings
;;   First, we’ll go over how to do the transformation, since the method which we
;; pick there determines the internal data representation. Then we’ll look at
;; how to go to and from that to the representation provided in the problem.

;;; Internal Representation
;;   So, Clojure isn’t strongly array-based. So representing the board as a big
;; double array and then iterating over each cell in the array to form a new one
;; and swapping them, like I did in High School with turtle graphics or in
;; college again in the intro to java course isn’t going to be the preferred
;; method because that gets all icky dealing with tons of array indices and a
;; double loop and ugh. So, representing it as a vector of vectors isn’t going
;; to be the preferred method here.
;;   Now, what you could do to follow roughly the same approach is use a map.
;; Generate all x-y pairs, which are your keys, and then store whether they’re
;; alive or dead in the values. So, if you had a 4x4 board as follows:
;;     . .
;;     # .
;;   your map would be:
;;     {[0 0] false [0 1] true [1 0] false [1 1] true}
;;   Then, you could map over the map (but not by using map!) and check for the
;; number of adjacent live cells, choosing the status depending on the count.
;; This is a perfectly legitimate way of doing it.
;;   However, you could further simplify the problem by only ever storing live
;; cells. For example, we could also represent the above 4x4 board by using a
;; set of all live cells, like follows:
;;     #{[0 1]}
;;   This is smaller (not that it matters with our current setup, since memory is
;; definitely not an issue) and doesn’t complicate anything - instead of
;; checking get, you check contains. Furthermore, you aren’t limited to a board
;; size! You can theoretically keep going, forever. The above implementation
;; doesn’t do this (rather, it checks every space on the board) but you could do
;; this by generating from the set of living cells the set of all cells adjacent
;; to a living cell, and then checking those, as well as the living cells. This
;; would allow you to handle arbitrarily large board sizes.
;;   So, that’s the representation that I settled on. You could also do the
;; traditional array-of-arrays, but I’d rather not do C++ in Clojure.

;;; ----------==========##########==========----------
;;;                Transforming The Board
;;; ----------==========##########==========----------
;;   How do we transform the board, now that we’ve got a set of all live cells?
;; Let’s examine what I’m doing already:
(let [y-max (count board)
      x-max (count (first board))
      adj-coords
        (fn [[x y]]
          (for [x-n [(dec x) x (inc x)]
                y-n [(dec y) y (inc y)]
                :when (not= [x-n y-n] [x y])]
          [x-n y-n]))
      count-adj
        (fn [[x y] cells]
           (let [adj (adj-coords [x y])]
             (reduce #(if (contains? cells %2) (inc %1) %1) 0 adj)))
      lives?
        (fn [[x y] cells]
          (let [num-live (count-adj [x y] cells)]
            (if (contains? cells [x y])
              (boolean (#{2 3} num-live))
              (boolean (#{3} num-live)))))
      step
        (fn [cells]
          (for [x-n (take x-max (range))
                y-n (take y-max (range))
                :when (lives? [x-n y-n] cells)]
            [x-n y-n]))
  ...)
;;   So, the way I wrote this was by actually using a repl, which lets you do
;; defn. So, if you look at the scratch, you’ll notice that these were
;; originally standalone functions. Since 4Clojure doesn’t let you use def, I
;; have to smoosh them together into a let.
;;   What it’s basically doing is:
;;   * For each cell on the board, call lives?; return the living ones
;;   * lives? counts the number of adjacent cells utilizing count-adj, and if
;; there are 2 or 3, it lives; otherwise, no
;;   * count-adj uses adj-coords to find all adjacent cells, and then counts
;; how many are living
;;   * adj-coords uses a for to generate coordinates adjacent to the ones
;; passed in
;;   I think the algorithm is solid, but we could definitely golf around the
;; code a bit to get the code size down. While we’re at it, we can convert the
;; step from using "For each space on the board" to "For all squares adjacent to
;; one or more."

;;   We’ll leave the adj-coords function alone, since I like the way it looks and
;; think it’s fairly clear and reasonably concise and can’t think of an obvious
;; way to change it without changing the clarity.

;;   As far as the count-adj and lives? functions go, we can actually just roll
;; them into one:
lives?
  (fn [[x y] cells]
    (let [num-live (count (clojure.set/intersection (into #{} (adj-coords [x y]))
	                                                cells))]
      (if (contains? cells [x y])
          (boolean (#{2 3} num-live))
          (boolean (#{3} num-live)))))

;;   The step uses another for loop to check each square dictated by the board as
;; a possible square. Let’s change that to checking each square adjacent to an
;; already living square:
step
  (fn [cells]
    (for [target (reduce #(apply conj %1 (adj-coords %2)) #{} cells)
	      :when (lives? target cells)]
	  target))
;;   That does work, but it’s kind of annoying me that we’re actually running
;; multiple adj-coords. See, we run adj-coords on each of the cells, then when
;; we go and check if all adjacent cells are living, we run adj-coords again. I
;; wonder if we can actually use the adj-coords - oh, wait, of course we can!
;; See, we’re losing data when we run the adj-coords on all living cells, but
;; plaster them into a set, thereby losing data. What we want to do is actually
;; map the number of times the cells appear in the adjacent list. If it appears
;; twice, it’s adjacent to two cells, thrice and three cells, and so on, and so
;; on.
;;   So, we should actually use a map here:
step
  (fn [cells]
    (for [[c n] (frequencies (reduce #(apply conj %1 (adj-coords %2)) [] cells))
	      :when (if (contains? cells c)
                    (boolean (#{2 3} n))
                    (boolean (#{3} n)))]
	  c))
;;   And that actually allows us to completely eliminate the lives? function.
;; However, this looks ugly and janky, so let’s see what we can do to clean it
;; up.
step
  (fn [cells]
    (for [[coords n] (frequencies (mapcat adj-coords cells))
	      :when (if (contains? cells coords)
		            (or (= n 2) (= n 3))
					(= n 3))]
	  coords))
;;   That...also looks kinda janky. The reduce there was a mapcat (I use reduce
;; for everything in the first pass, which is kind of a habit thing) and the
;; check-for-presence-in-a-set got changed to a more conventional or equals for
;; reasons of readability. Well, half the jankiness comes from having to stick
;; it in a let. We’ll leave this here, and move on to the code to convert the
;; input array of strings into a set of live cells.

;;; ----------==========##########==========----------
;;;           Converting To A Set Of Cells
;;; ----------==========##########==========----------
;;   The existing code uses a pair of functions:
collapse-str
  (fn [s y]
	(->> s
		 (map #(if (= \# %2) [%1 y] nil) (take (count s) (range)))
		 (filter #(not= % nil))
		 (into #{})))
;;   collapse-str takes [string int], with the int as the y position of the
;; string in the array, and returns a set of populated cells in the form of [x
;; y]. It’s called by collapse-vec, which is responsible for determining the
;; y-position of each string, and which will combine the results into the final
;; set of all populated cells.
;;   As far as actual code goes, this is...uh, hmm. You know what, we’re doing a
;; cobbled-together map-indexed here. Huh, apparently I didn’t know about that
;; function when I wrote this. Also? I’m not sure it needs to be in a set - the
;; result will be smooshed into a different data structure upon the point at
;; which we combine them, so why put it into a set? In fact, I’m positive it
;; doesn’t; let me look up for a second. Yup, it doesn’t.
collapse-str
  (fn [s y]
	(->> s
	     (map-indexed #(if (= \# %2) [%1 y] nil))
		 (filter #(not= % nil))))
;;   You know, it’s not really necessary to use the nesting form now that I’ve
;; eaten a few of the steps. I wonder how it looks without?
collapse-str
  (fn [s y]
	(filter #(not= % nil)
	        (map-indexed #(if (= \# %2) [%1 y] nil) 
			             s))))
;;   Eh. To taste, I suppose. I’d personally go with the nesting form, but hey,
;; whatever. Also we’ve kinda got a mapcat thing going here, what with the nils,
;; but if we want to use mapcat instead of filter we’ve got to drop map-indexed
;; and go back to manually adding in an index collection. You’d end up with a
;; rather long one-liner but obviously you can break it up if you like:
collapse-str
  (fn [s y]
    (into #{} (mapcat #(if (= \# %2) [[%1 y]] nil) (range (count s)) s)))
;;   Enough screwing around with collapse-str; these are pretty much equivalent.
;; It’s mostly a matter of which you like the most.

;;   Now for collapse-vec:
collapse-vec
  (fn [v]
	(->> v
		 (map #(vector %1 %2) (reverse (take (count v) (range))))
		 (reduce #(concat %1 (collapse-str (last %2) (first %2))) [])
		 (into #{})))
;;   Okay, hold on, before we move on I have to correct that dumb (take (count v)
;; (range)) thing I have going on:
collapse-vec
  (fn [v]
	(->> v
		 (map #(vector %1 %2) (reverse (range (count v))))
		 (reduce #(concat %1 (collapse-str (last %2) (first %2))) [])
		 (into #{})))
;;   So first, let’s examine the transformations. We’ve got a map-indexed thing
;; going on in the first transformation, but because of the fact that rows go
;; from top-down and the traditional math-y indexing system goes bottom up,
;; there’s a reverse. We could still replace that with map-indexed, though; it’d
;; just add in a reverse.
;;   Then, we’ve got a reduce. Note that the reduce has a concat in it. So, map
;; plus a reduce that has a concat in it - obvious replacement would be a
;; mapcat, right?
collapse-vec
  (fn [v]
	(->> (mapcat collapse-str v (reverse (range (count v))))
		 (into #{})))
;;   Mix and match syntax to taste, what with the threading and into that you can
;; reconfigure however you like. I’ll go with the following
collapse-vec
  (fn [v] (into #{} (mapcat collapse-str v (reverse (range (count v))))))
;;   because I hate readability and am the enemy of all things good. No, no, I’m
;; joking, I’d probably go with
collapse-vec
  (fn [v]
	(->> (range (count v))
         (reverse)
	     (mapcat collapse-str v)
		 (into #{})))

;;; ----------==========##########==========----------
;;;      Converting From Set To Array Of Strings
;;; ----------==========##########==========----------
;; Finally, we’ve got explode-set:
explode-set
  (fn [s]
	(let [spaces-list (into [] (take x-max (repeat \space)))
		  y-range (take y-max (range))
		  live-map
			(reduce (fn map-set-to-rows [mp st]
					  (assoc mp
						(last st)
						(conj (get mp (last st)) (first st))))
					(zipmap y-range (take y-max (repeat [])))
					s)
		  explode-str
			(fn [s-e]
			  (->> s-e
				   (reduce #(assoc %1 %2 \#) spaces-list)
				   (apply str)))]
	  (->> y-range
		   (into [])
		   (map #(explode-str (get live-map %)))
		   (reverse))))
;;   Hmm. That’s rather longer than is ideal! What was it that the style guide
;; says about function length - try to keep them to ten lines? Well, the result
;; of this function being huge is that 4Clojure doesn’t like defn, so I’ve
;; crammed a few functions into one, here. Let’s go through the two
;; sub-functions, and then return to explode-set:

;;   live-map - maps the set of live cells to their rows
live-map
  (reduce (fn map-set-to-rows [mp [x y]]
	        (assoc mp y (conj (get mp y) x)))
		  (zipmap y-range (take y-max (repeat [])))
		  s)
;;   I edited it so it uses destructuring instead of first and last, so it’ll be
;; a tad more clear. live-map is a mapping of x-values to y-values; for example,
;; if we have a set, s, consisting of #{[0 0] [0 1] [1 1]} in a 3x3 board,
;; live-map will return {0 [0], 1 [1 0], 2 []}. The way it does it is utilizing
;; assoc, with a default value of []. Looks a bit confusing but it’s really not
;; that bad. Unfortunately it’s not very readable. Could we do it differently?
;;   Well, basically, we’re mapping by one of the elements of the 2-integer
;; vector, right? So we could use group-by instead. Unfortunately I can’t think
;; of a group-by-and-transform function, so after we do that we’ll have to
;; re-map it, but we could do it this way:
live-map
  (into {} (map (fn [[k v]]
                  [k (into [] (map first v))]) 
                (group-by second s)))
;;   Aaaand here’s probably a good place to use ->>:
live-map
  (->> (group-by second s)
       (map (fn [[k v]] [k (into [] (map first v))]))
	   (into {}))

;;   explode-str - I rolled spaces-list into the function:
explode-str
  (fn [s-e]
    (->> s-e
	     (reduce #(assoc %1 %2 \#)
		         (into [] (take x-max (repeat \space))))
	     (apply str)))
;;   What this basically does is creates a vector of spaces [x-length of board]
;; long, and then replaces the indicies where there exists a living cell with
;; \#. Then it turns it into a string. Aside from the fact that the huge reduce
;; is rather ugly, I think that this section of code works just fine.
;;   That leaves us with our new explode-set:
explode-set
  (fn [s]
	(let [live-map
            (->> (group-by second s)
                 (map (fn [[k v]] [k (into [] (map first v))]))
	             (into {}))
		  explode-str
            (fn [s-e]
              (->> s-e
	               (reduce #(assoc %1 %2 \#)
		                   (into [] (take x-max (repeat \space))))
	               (apply str)))]
	  (->> (take y-max (range))
		   (into [])
		   (map #(explode-str (get live-map %)))
		   (reverse))))
;; The actual code, take, into [], map and reverse, is basically trivial.

;;; ----------==========##########==========----------
;;;                The Final Function
;;; ----------==========##########==========----------
;;   Putting it all together:
(fn life [board]
  (let [y-max (count board)
        x-max (count (first board))
        adj-coords
          (fn [[x y]]
            (for [x-n [(dec x) x (inc x)]
                  y-n [(dec y) y (inc y)]
                  :when (not= [x-n y-n] [x y])]
            [x-n y-n]))
        step
          (fn [cells]
            (for [[coords n] (frequencies (mapcat adj-coords cells))
	              :when (if (contains? cells coords)
		                    (or (= n 2) (= n 3))
					        (= n 3))]
	          coords))
        collapse-str
          (fn [s y]
            (into #{} (mapcat #(if (= \# %2) [[%1 y]] nil) 
			                  (range (count s)) s)))
        collapse-vec
          (fn [v] (into #{} (mapcat collapse-str 
		                            v
									(reverse (range (count v))))))
        explode-set
          (fn [s]
			(let [live-map
                    (->> (group-by second s)
                         (map (fn [[k v]] [k (into [] (map first v))]))
	                     (into {}))
		          explode-str
                    (fn [s-e]
                       (->> s-e
	                       (reduce #(assoc %1 %2 \#)
		                           (into [] (take x-max (repeat \space))))
	                       (apply str)))]
	          (->> (take y-max (range))
		           (into [])
		           (map #(explode-str (get live-map %)))
		           (reverse))))]
                      (->> y-range
                           (into [])
                           (map #(explode-str (get live-map %)))
                           (reverse))))]
    (explode-set (step (collapse-vec board)))))
;;   It’s 45 lines long, down from 58. Either way? Too huge. The actual "Do Game
;; Of Life" code, however (just the step and adj-coords) is only 13 lines long -
;; quite concise, really! You could probably get that down a little, or compress
;; it (it’s still in a let, which eats up two extra lines) but hey, that’s not
;; bad.