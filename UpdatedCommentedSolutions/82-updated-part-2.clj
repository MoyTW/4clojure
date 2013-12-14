;;;; 82 - Word Chains - Part 2
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/82-wip.clj
;; Original:
(fn oh-god-no [s]
  (letfn [(diff-by-del [long-w short-w]
            (loop [n (count long-w)]
              (cond
               (= n 0) false
               (= (str (subs long-w 0 (dec n)) (subs long-w n)) short-w) true
               :else (recur (dec n)))))
          (diff-by-sub [l-w r-w]
            (if (= 1 (reduce #(if %2 %1 (inc %1)) 
                             0 
                             (map #(= %1 %2) l-w r-w)))
              true
              false))
          (diff-by-one [l-w r-w]
            (let [l-cnt (count l-w) r-cnt (count r-w)]
              (cond 
               (= l-cnt r-cnt) (diff-by-sub l-w r-w)
               (= (inc l-cnt) r-cnt) (diff-by-del r-w l-w)
               (= (inc r-cnt) l-cnt) (diff-by-del l-w r-w)
               :else false)))
          (find-one-shifts [wrd st]
            (filter #(diff-by-one wrd %) st))
          (set-to-sorted-map [s]
            (let [unsorted (reduce #(assoc %1 %2 (find-one-shifts %2 s)) {} s)]
              (into (sorted-map-by #(compare [(count (get unsorted %1)) %1]
                                             [(count (get unsorted %2)) %2]))
                    unsorted)))]
    (let [get-lowest-count (fn [st mp] (first (sort-by #(count (get mp %1)) st)))
          mp (set-to-sorted-map s)]
      (loop [head (last (first mp)) mp (dissoc mp (ffirst mp))]
        (let [p-lnks (filter #(contains? mp %) head)]
          (cond
           (empty? mp) true
           (empty? p-lnks) false
           :else
           (let [next-key (get-lowest-count p-lnks mp)]
             (recur (get mp next-key) (dissoc mp next-key)))))))))
;;   For an analysis and rewriting of everything in the letfn, go to
;; https://github.com/MoyTW/4clojure/blob/master/UpdatedCommentedSolutions/82-updated-part-1.clj
;; - this part will concern how to check if we can, in fact, chain the words
;; together.

;;   This is the section that concerns us:
(fn oh-god-no [s]
  (...
    (let [get-lowest-count (fn [st mp] (first (sort-by #(count (get mp %1)) st)))
          mp (set-to-sorted-map s)]
      (loop [head (last (first mp)) mp (dissoc mp (ffirst mp))]
        (let [p-lnks (filter #(contains? mp %) head)]
          (cond
           (empty? mp) true
           (empty? p-lnks) false
           :else (let [next-key (get-lowest-count p-lnks mp)]
                   (recur (get mp next-key) (dissoc mp next-key)))))))))
;;   Before we go and toss the old code in favor of a completely different
;; algorithm, let's examine what it's doing. First we bind mp (not an amazing
;; name, I know) to a map with words mapped to a set of adjacent words, sorted
;; by the number of adjacent words.
;;   The get-lowest-count function takes a set of words and the map, and chooses
;; from the map the word which has the lowest number of adjacent words.
;;   The actual processing occurs in the loop/recur, which takes a head and a
;; map. It actually destructs the map as it passes through. Each pass-through
;; removes from the map the current head and moves to the next head in priority
;; of number of outgoing links. If you've consumed the whole map, then that
;; means that the chain extends across the entire map - if you haven't consumed
;; the map, but have run out of outgoing links, then that means there's no
;; chain.
;;   The major issue with this algorithm is that it's a heuristic search -
;; basically it just takes the next node with the lowest number of outgoing
;; nodes but makes no effort to backtrack if it hits a dead end! For example,
;; examine the following graph:
;;         [XX]    [VV]
;;          |       |
;;  .------[XVX]---[XVV]
;;  |       |       |
;; [VXVX]--[XXVX]--[XXVV]
;;  |     / |       |
;; [XVXVX]-[XXXVX]-[XXXVV]
;;   There's clearly a chain here - we can go XX -> XVX -> XXVX -> VXVX -> XVXVX
;; -> XXXVX -> XXXVV -> XXVV -> XVV -> VV! But, unfortunately, the heuristic
;; "Always pick the outgoing path which leads to the lowest-magnitude node" will
;; leads us to XX -> XVX -> VXVX -> XVXVX -> XXXVX -> XXXVV -> XXVV -> XVV ->
;; VV, leaving out the node XXVX with five edges!
;;   Apologies for the naming of the nodes. Might have done 0 and 1, maybe? I
;; just had an easier time visualizing it because Roman Numerals or something.
;;   Anyways, you can confirm that this is true and that there are cases in which
;; the heuristic fails to find a path when one exists by def-ing the oh-god-no
;; function and running the following:
(oh-god-no #{"XX" "VV" "XVX" "XVV" "VXVX" "XXVX" "XXVV" "XVXVX" "XXXVX" "XXXVV"})

;;   So, unfortunately, the algorithm used to detect paths is fatally flawed!
;; Usually I'd knock around the code and play a little golf but the algorithm
;; itself is totally unfit for purpose, so let's just toss this all out and
;; start over again. How might we detect a word chain?
;;   An astute reader may note that the map of adjacent words is actually a
;; graph, and a word chain is a path which visits each node once. In other
;; words, we're checking for the existence of a Hamiltonian path, aaaand I'm
;; pretty sure that's NP-complete, so there's not likely to be an elegant
;; solution which also fits within the scope of a 4Clojure problem. So how about
;; just doing an exhaustive search? Those are pretty simple, and we could do
;; something like a DFS/BFS from each node, and if it visits all the nodes,
;; we've got a path.
;;   Yes, it's terribly inefficient, but this isn't designed to be robust over
;; large datasets.

;;   So, a function to detect whether there's a path from a given node:
(
(fn has-path [graph node visited]
  (let [outgoing (clojure.set/difference (set (graph node)) visited)
        visited (conj visited node)]
    (cond (empty? outgoing) (= (count visited) (count (keys graph)))
          :else (map #(has-path graph % (conj visited %)) outgoing))))
{"XXVV" ["XXVX" "XVV" "XXXVV"], "VV" ["XVV"], "XX" ["XVX"], "XXVX" ["XXVV" "VXVX" "XVX" "XXXVX" "XVXVX"], "VXVX" ["XXVX" "XVX" "XVXVX"], "XVV" ["XXVV" "VV" "XVX"], "XXXVV" ["XXVV" "XXXVX"], "XVX" ["XX" "XXVX" "VXVX" "XVV"], "XXXVX" ["XXVX" "XXXVV" "XVXVX"], "XVXVX" ["XXVX" "VXVX" "XXXVX"]} "XX" #{})
;;   This is a very simple non-repeating DFS search from a given node through the
;; whole graph. It returns whether, when it's exhausted the entire graph, it's
;; visited every node. Or, rather, it returns a map, containing the results of
;; all the child paths. Hmm, that's less than ideal! Really, we'd rather just a
;; yes/no on whether any of the child paths found a full-graph path. Let's
;; change that:
(fn has-path [graph node visited]
  (let [outgoing (clojure.set/difference (set (graph node)) visited)
        visited (conj visited node)]
    (cond (empty? outgoing) (= (count visited) (count (keys graph)))
          :else (some identity (map #(has-path graph % (conj visited %)) outgoing)))))
;;   There we go.

;;   We can then integrate it into our code by just pasting it in and then
;; mapping over our keys (nodes) like so:
(fn oh-god-no [s]
  (...
    (letfn [(has-path [graph node visited]
              (let [outgoing (clojure.set/difference (set (graph node)) visited)
                    visited (conj visited node)]
                (cond (empty? outgoing) (= (count visited) (count (keys graph)))
                      :else (some identity (map #(has-path graph % (conj visited %)) outgoing)))))]
      (let [g (set-to-sorted-map s)]
        (true? (some identity (map #(has-path g % #{}) (keys g))))))))
;;   Note the true? at the end there. That's to eliminate nil values, since the
;; unit tests aren't truthy but rather check specifically for true/false, and
;; some returns nil instead of false if there are none.
;;   Also note that this is pretty hideously inefficient. It continues checking
;; nodes even if (has-path) hits a node for which there is a path, for example,
;; and uses a brute-force method to find the path, and oh dear! This is
;; inefficient as heck! However, the problem sets are relatively small, and so I
;; feel comfortable doing it this way so long as there's a disclaimer - so
;; disclaimer made. Not to be used in Important Production Code, and such, etc,
;; etc.

;; Our final code, after all the changes, is the following:
(fn nineteen-lines! [s]
  (letfn [(lev-dist [left right]
            (let [cost (if (= (last left) (last right)) 0 1)]
              (cond (= 0 (count left)) (count right)
                    (= 0 (count right)) (count left)
                    :else (min (inc (lev-dist (butlast left) right))
                               (inc (lev-dist left (butlast right)))
                               (+ cost (lev-dist (butlast left) (butlast right)))))))
          (find-one-shifts [wrd st]
            (filter #(= 1 (lev-dist wrd %)) st))
          (set-to-sorted-map [s]
            (reduce #(assoc %1 %2 (find-one-shifts %2 s)) {} s))
          (has-path [graph node visited]
            (let [outgoing (clojure.set/difference (set (graph node)) visited)
                  visited (conj visited node)]
              (cond (empty? outgoing) (= (count visited) (count (keys graph)))
                    :else (some identity (map #(has-path graph % (conj visited %)) outgoing)))))]
      (let [g (set-to-sorted-map s)]
        (true? (some identity (map #(has-path g % #{}) (keys g)))))))
;;   As the name implies, it's down to nineteen lines, from 37. Unfortunately
;; it's actually much slower, but on the other hand, it won't miss
;; strangely-formed word chains. All in all I think the revised function is much
;; superior to the original. You can golf it around a little but more; there are
;; a couple of ugly-looking #()s, the cond there in has-path would be served
;; just as well by an if, some of the heavy nesting is a little ugly...nah,
;; let's move on. We've done enough on this one already.