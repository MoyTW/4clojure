;;;; 79 - Triangle Minimal Path
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/79-wip.clj
;; Original:
(fn min-tri [coll]
  (letfn [(expand [coll]
                  (reduce (fn [v n]
                            (if (coll? (last v))
                              (conj (pop v) (conj (last v) n) [n])
                              [[n] [n]]))
                          []
                          coll))
          (next-level [t-c b-c]
                      (map #(+ (apply min %1) %2) t-c b-c))]
  (apply min (reduce #(next-level (expand %1) %2) coll))))
;;   So, before we examine the code, let's talk about the triangle we're supposed
;; to be finding a minimal path through. Go look at the 4Clojure problem. We'll
;; use the second triangle as an example.
;;   Here is the triangle, left-aligned:
;;     [3]
;;     [2  4]
;;     [1  9  3]
;;     [9  9  2  4]
;;     [4  6  6  7  8]
;;     [5  7  3  5  1  4]
;;   Note that the valid paths downwards from index i are [i, i+1]. So, for the 9
;; in the 2nd row down, the possible moves are the 9 and the 2 in the 3rd row.
;; It's basically a weighted digraph, but constructed in such a way that the
;; nodes are arranged in very neat little rows.
;;   So, my go-to algorithm for finding optimal solutions of weighted digraphs is
;; Djikstra's algorithm, because that's the only one I can consistently
;; remember. That's a less than stunning reason for picking it, I know, but we
;; can apply it here in a modified fashion.
;;   Because the graph always goes one way with a predictable expansion pattern,
;; we actually don't need to keep the whole graph; we can discard rows which
;; we've already summed. So instead of marking nodes as visited, we can just
;; toss them out. Also we don't actually want the path, we just want the sum, so
;; we can forget about tracking the nodes themselves and just track the cost.
;; All this means that the actual algorithm is very simple.
;;   We reduce over the triangle. When processing a new row, we map the cost of
;; the new row to the lowest path that leads to it (there are always either 1 or
;; 2). This gives us a running total of the costs. When we finally get to the
;; end, we pick the lowest cost.
;;   It's basically an extraordinarily truncated adaptation of Djikstra's
;; algorithm. Efficiency-wise, comparing number of mins against number of rows
;; n, it has the 1+2, 1+2+3 pattern, giving us a number of comparisons around
;; (average(n)*n). It's not quite n*n, but it does follow an accelerating curve.
;;   Then again, if you measure your input set by actual number of values instead
;; of number or rows it comes out linear; it's just that each row is determined
;; to have one more piece of data than the last.
;;   As a side note I always felt really bad about throwing away all the extra
;; data in those efficiency analysis exercises.
;;   I'm not sure how you could get it any lower and still have an assured ideal
;; solution, though - you're going to have to check every value going down if
;; you want to be sure. So I'll put it up as "about as efficient as one might
;; hope for" until somebody comes along and blows that out of the water.
;;   Anyways, enough about the algorithm, let's look at the code. We'll start
;; with the sub-function to expand the above row into the possible paths to the
;; next row:
(expand [coll]
  (reduce (fn [v n]
            (if (coll? (last v))
                (conj (pop v) (conj (last v) n) [n])
                [[n] [n]]))
          []
          coll))
;;   Well, well, well, that's definitely a sign that I wrote the function inside
;; and then pasted it into the reduce, because there's a clumsy if to handle the
;; special case of the first iteration. We can simply pre-prime it, as follows:
(expand [coll]
  (reduce (fn [v n] (conj (pop v) (conj (last v) n) [n]))
          [[(first coll)] [(first coll)]]
          (rest coll)))
;;   I do have an issue with the reduce function being a confusing morass of conj
;; statements, but the equivalent assoc syntax doesn't really improve the
;; situation, being wordier than the (conj (pop) (...) (...)) syntax. Either way
;; it's a pretty confusing set of statements there, which may or may not be
;; helped by additional spacing, as such:
(expand [coll]
  (reduce (fn [v n] 
              (conj (pop v) 
                    (conj (last v) n) 
                    [n]))
          [[(first coll)] [(first coll)]]
          (rest coll)))
;;   Actually, it does make it a little more clear. Really, I'm using conj a
;; little like cons here, but hey, vectors append at the end so...
;;   Anyways, I think getting the reduce function down to one line (depending on
;; whitespace) is good enough here. What about the rest of the code? Well,
;; looking at it right now, the remainder of the code is...a pair of one-line
;; statements. So, I think my final, updated version would be:
(fn min-tri [coll]
  (letfn [(expand [coll]
            (reduce #(conj (pop %1) (conj (last %1) %2) [%2])
                    [[(first coll)] [(first coll)]]
                    (rest coll)))
          (next-level [t-c b-c]
            (map #(+ (apply min %1) %2) t-c b-c))]
  (apply min (reduce #(next-level (expand %1) %2) coll))))
;;   Because I have a compactness fetish, it clocks in at eight lines (and I
;; abuse anonymous functions in ways which are Not Ideal), but if you want to
;; actually read it you might be better served by decompressing it.

;;;; 80 - Perfect Numbers
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/80-wip.clj
;; Original:
(fn [n]
  (= n (apply
        +
        (filter #(= 0 (mod n %))
                (rest (take n (range)))))))
;;   That's some, uh, very funny formatting that I've got going there. Hold on a
;; second.
(fn perfect-numbers [n]
  (= n 
     (apply +
            (filter #(= 0 (mod n %))
                    (rest (take n (range)))))))
;;   See, that's not much better, but what it does is make it abundantly clear
;; that we can use the threading macro here:
(fn perfect-numbers [n]
  (->> (range)
       (take n)
       (rest)
       (filter #(= 0 (mod n %)))
       (apply +)
       (= n)))
;;   Also? Why am I still doing range, take, rest when range has a perfectly good
;; set of optional parameters we can use:
(fn perfect-numbers [n]
  (->> (range 1 n)
       (filter #(= 0 (mod n %)))
       (apply +)
       (= n)))
;;   Okay, now that we've got some slimming down done without changing the
;; meaning of the number, what is this algorithm doing? First, it takes all
;; numbers from 1 to n (exclusive on n), and then it filters all the divisors.
;; Then it adds them and compares them to n, returning whether it's true or not
;; - if it's true, the number is perfect.
;;   As for the actual algorithm it's suboptimal to horrifyingly bad, because
;; literally modding every single number from 1 to n is inefficient as balls
;; compared to other, better ways of finding all the divisors. For starters, I
;; could go from (inc (/ n 2)), but a better way would be to implement some sort
;; of better algorithm that I would research and look up.
;;   However, for 4Clojure, it really, really, really isn't worth the effort to
;; do that. Well, we could do the "only check numbers under half of n," but the
;; other stuff? Nah. So, our final function is:
(fn perfect-numbers [n]
  (->> (range 1 (inc (/ n 2)))
       (filter #(= 0 (mod n %)))
       (apply +)
       (= n)))

;;;; 81 - Set Intersection
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/81-wip.clj
;; Original:
(fn cust-inter [lset rset]
  (reduce #(if (contains? rset %2) (conj %1 %2) %1)
          #{}
          lset))
;;   This is a real simple
;; possibly-one-liner-if-you-enjoy-pretending-clojure-is-perl. It basically just
;; goes through the left set, and if it finds a matching element in the right
;; set it tosses it into the new set and we're done.
;;   What other ways could we do this, though? How about using filter? Let's try
;; it out:
(fn cust-inter [lset rset]
  (set (filter rset lset)))
;;   That's even more compact! I like it!