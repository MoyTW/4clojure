;;;; 95 - To Tree, or not to Tree
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/95-wip.clj
;; Original:
(fn test-btree [[val l-chld r-chld :as node]]
  (cond
    (= node nil) true
    (or (not (or (= nil l-chld) (coll? l-chld)))
        (not (or (= nil r-chld) (coll? r-chld)))
        (= val nil)
        (not= (count node) 3))
      false
    :else (and (test-btree l-chld) (test-btree r-chld))))
;;   So, the data structure we want to check for is a btree, in the form of
;; [value left-child right-child].
;;   The existing function recursively checks each node. If the node is nil, it
;; returns true; if it's an invalid node, false; if it's a valid node, it
;; returns itself, called on the two child nodes. The only really interesting
;; thing here is the specific condition clause.
;;   The current method for checking if it's a node is checking all the ways in
;; which the node might be invalid, and clunkily too! We can clean that up a
;; little bit by moving the positive checks up, and letting the else catch all
;; the negatives:
(fn btree? [[val l-chld r-chld :as node]]
  (cond
    (= node nil) true
    (and (coll? node)
         (= (count node) 3))
      (and (btree? l-chld) (btree? r-chld))
    :else false))
;;   Unfortunately, this fails the second-to-last test! It gives a nth not
;; supported on this type. Why's that? Well, it's trying to destructure a false,
;; and that just won't do. So, we can solve this by cutting out the
;; destructuring clause, and putting in second and last in the recursive call -
;; and we can be sure that there will be a second and last representing what we
;; want because the condition checks that the count is three:
(fn btree? [node]
  (cond
    (= node nil) true
    (and (coll? node) (= (count node) 3))
      (and (btree? (second node)) (btree? (last node)))
    :else false))
;;   There we go.

;;;; 96 - Beauty is Symmetry
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/96-wip.clj
;; Original:
(fn check-sym [[val l-tree r-tree]]
  (letfn [(reverse-btree [[val l-chld r-chld :as node]]
           (if (= node nil) nil
             [val (reverse-btree r-chld) (reverse-btree l-chld)]))]
    (= l-tree (reverse-btree r-tree))))
;;   This utilizes the same node structure as the last one, and checks if the
;; tree is symmetrical. The quickest way that sprung to mind was that we could
;; simply recursively reverse one half of the tree, so I wrote that.
;;   We assume that the btree is well-formed, so we don't have to deal with bad
;; input. I guess we could put in btree? if we wanted to check for input
;; structure, but we'll assume we don't.
;;   The code to reverse the btree is in a letfn:
(reverse-btree [[val l-chld r-chld :as node]]
  (if (= node nil) nil
    [val (reverse-btree r-chld) (reverse-btree l-chld)]))
;;   It's basically trivial; if the node is nil, it returns nil. Otherwise, it
;; simply calls itself on its children and returns them swapped. There's not
;; actually a ton that I feel I can rewrite here, apart from maybe naming, and
;; really, it's not worth rewriting it just because of the naming...

;;;; 97 - Pascal's Triangle
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/97-wip.clj
;; Original:
(fn nth-row [n]
  (letfn [(next-row [row]
            (let [n-row (reduce (fn [[out l] n]
                                  [(conj out (+ l n)) n])
                                [[] 0]
                                row)]
              (conj (first n-row) (last n-row))))]
    (nth (iterate next-row [1]) (dec n))))
;;   I remember doing something with this in middle school math, I think. It's
;; also very similar to the triangle path problem (#79). The triangle actually
;; follows the same pattern:
;;     1
;;     1 1
;;     1 2 1
;;     1 3 3 1
;;     1 4 6 4 1
;;   Notice that the value of index i is previous-row[i] + previous-row[i - 1]!
;; So, that's a pretty easy algorithm, which turns rather nicely into a reduce.
;; We basically step over the previous row, two elements at a time, adding them
;; as we go, like follows:
;;       1     2     1
;;     [0+1] [1+2] [2+1] = 1 3 3
;;   It also stores the last integer, which is always 1, in the second element of
;; the returned vector. So, the result of the reduce here would actually be [[1
;; 3 3] 1].
;;   The function that implements it is...kind of confusing, actually:
(reduce (fn [[out l] n]
          [(conj out (+ l n)) n])
        [[] 0]
        row)]
;;   Maybe renaming the variables here might help:
(reduce (fn [[output last-integer] next-integer]
          [(conj output (+ last-integer next-integer)) next-integer])
        [[] 0]
        row)]
;;   last-integer holds the previous-row[i-1] as the reduce steps across the
;; input row. When it reaches the end, it returns
;; [[next-row-of-triangle-lacking-one-member] 1]. So, to actually get the next
;; row of the triangle, we add on that remaining 1.
;;   As horrible as it looks...I'm not sure how I could make it look prettier.
;; Hmm. I mean, I could rewrite it differently:
(fn nth-row [n]
  (nth (iterate (fn [row] (concat (map + (concat [0] row) row) [1])) [1]) (dec n)))
;;   Hahaha, okay, here's a more readable version:
(fn nth-row [n]
  (nth (iterate (fn [row] 
                  (concat (map + 
                               (concat [0] row) 
                               row) 
                          [1])) 
                [1]) 
       (dec n)))
;;   Oh wait I LIED. How about...
(fn nth-row [n]
  (let [next-row (fn [row] 
                   (concat (map + 
                                (concat [0] row) 
                                row)
                           [1]))]
    (nth (iterate next-row [1]) (dec n))))
;;   Eh. Well. Whatever. Turns out all my solutions to this one? Nigh-unreadable.
;; Sad day. I wonder how other people did it, and if their solutions were
;; readable?