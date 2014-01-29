;;;; 105 - Identify keys and values
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/105-wip.clj
;; Original:
(fn build-map [kvs]
  (let [red-func
          (fn [[mp lst] nxt]
            (cond
              (and (keyword? lst) (keyword? nxt)) [(assoc mp lst []) nxt]
              (keyword? lst) [(assoc mp lst nxt) nxt]
              :else [mp nxt]))
        grouped-vals
          (mapcat #(if (keyword? (first %)) % [%])
                  (partition-by keyword? kvs))]
    (first (reduce red-func [{} nil] grouped-vals))))
;;   So, this problem provides what's basically a flattened map (though, not with
;; the flatten function, since it won't actually work on maps), except that
;; instead of blank containers some keys just have nothing at all. We want to
;; turn it into a proper map.
;;   For example, look at [:a 1 2 3 :b :c 4]; this should become {:a [1 2 3] :b
;; [] :c [4]}. That looks simple enough, right? Just pull out the keywords and
;; the integers behind them, and that would work super well except for the empty
;; elements. So, unfortunately, this makes it a lot harder.
;;   The algorithm I ended up using first groups the integers into one element,
;; storing them in grouped-vals, and then reduces its way through the values two
;; at a time. If it encounters a two keywords in a row, it adds they keyword to
;; the map with an empty element. If it encounters a collection of integers and
;; a keyword, it adds the keyword in with those integers. Otherwise, it
;; continues traversing the list.
;;   Unfortunately I haven't yet figured out a good general-purpose algorithm for
;; "go through a list, taking two elements at a time," so whenever I need to do
;; something like that I just end up doing a reduce with a look-behind element.
;; I suppose you could do a list comprehension, and then map over it or
;; something like that?
;;   You could also run some sort of map/mapcat which would find any elements
;; which are composed solely of keywords, after partitioning them, like so:
(mapcat
  #(if (and (<= 2 (count %)) (every? keyword? %))
       (interpose [] (apply vector %))
       [%])
  ['(:a) '(1 2 3) '(:b :c) '(4)])
;;   Unfortunately this turns out to be a little more difficult than one might
;; hope, because of the fact that you actually need to check data across the
;; element boundaries - for example, to tell if you need to add one or two []
;; elements to '(:b :c), you have to know whether it's followed by an integer or
;; not! map/mapcat isn't good at dealing with these kinds of cross-element
;; checks - and sadly reduce isn't great, either. You can make it work - see
;; above - but a much cleaner approach is loop-recur. Observe:
(loop [out {} in [:a 1 2 3 :b :c 4]]
  (if (not (empty? in))
      (let [k (first in)
            v (take-while integer? (rest in))]
        (recur (assoc out k v) (drop-while integer? (rest in))))
      out))
;;   We can turn it into a proper solution easily:
(fn build-map [keyvals]
  (loop [out {} in keyvals]
    (if (seq in)
        (recur (assoc out (first in) (take-while integer? (rest in)))
               (drop-while integer? (rest in)))
        out)))
;;   I think that a loop/recur is more suited to this particular problem because
;; it has a very irregular, difficult-to-handle structure, so while I'm normally
;; not terribly enthusiastic about loop/recur and like to replace it with
;; standard functions whenever possible...I think it's a better solution here.

;;;; 106 - Number Maze
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/106-wip.clj
;; Original:
(fn search [nd-fst nd-lst]
  (letfn [(gen-next [st]
             (into #{}
               (mapcat (fn [n]
                         (if (odd? n) [(* n 2) (+ n 2)]
                         [(* n 2) (/ n 2) (+ n 2)]))
                       st)))]
  (loop [i 1 node-set #{nd-fst}]
    (if (contains? node-set nd-lst) i
      (recur (inc i) (gen-next node-set))))))
;;   Before you can write code to solve the problem, you've got to solve the
;; problem itself! Fortunately I actually remembered something from my college
;; days. It was an AI pathing problem, actually - you've got a bunch of
;; 'squares' and certain ways you can move between the squares, and you need to
;; find a path from A to B. Well, here, we've got 'squares' (each integer is a
;; square) and 'paths' (operations, leading to different 'squares'). So the
;; problem is actually quite simple, once you remap it into a pathfinding
;; problem! Except it's not actually a pathfinding problem, because we don't
;; save the path - all we want is the distance.
;;   This makes it basically trivial. All we need to do is basically a blind,
;; memory-less DFS, with the only thing saved being the current set of squares
;; and the steps we've taken. So, that's what it does. gen-next blows up the
;; current set of steps into the next set of steps, and then there's a trivial
;; loop/recur with the end condition being having found the targeted end node.
;;   Algorithmically, there's nothing wrong with it (unless you have something
;; against exhaustive searches, I guess), but it's, uh, it's very ugly.
(fn search [start end]
  (letfn [(next-paths [paths]
             (into #{}
               (mapcat (fn [n]
                         (if (odd? n) 
                             [(* n 2) (+ n 2)]
                             [(* n 2) (/ n 2) (+ n 2)]))
                       paths)))]
  (loop [i 1 paths #{start}]
    (if (contains? paths end) i
      (recur (inc i) (next-paths paths))))))
;;   I mean, come on, nd-fst and nd-lst? That's some serious nonhelpful shorthand
;; there! Also, the fact that there are functions nested in functions due to a
;; lack of def most definitely makes it uglier...

;;;; 107 - Simple closures
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/107-wip.clj
;; Original:
;;;; -----=====***** 107 (2 lines, 23 total) *****=====-----
(fn build-func [n]
  (fn f-of-x [x] (int (Math/pow x n))))
;;   Presented without comment.