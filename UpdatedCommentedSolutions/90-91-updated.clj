;;;; 90 - Cartesian Product
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/90-wip.clj
;; Original:
(fn cross-product [lset rset]
  (reduce
    (fn r [s n]
      (apply conj s (reduce #(conj %1 [%2 n]) #{} lset)))
    #{}
    rset))
;;   Okay, we've got a double-reduce here and extremely awkward spacing, what
;; with the parameters to reduce going down a line instead of to the right.
;;   So, basically, the double reduce is basically a double for, adding to the
;; accumulated value in the same fashion you'd do with a double-for in, say, C#.
;; There's...probably a better way to do this, since that's awkward as heck.
;;   We could use for, like so:
(fn cross-product [lset rset]
  (into #{} (for [l lset r rset] [l r])))
;;   It's pretty much one line, huh? Well, there we go. That works.
        
;;;; 91 - Graph Connectivity
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/91-wip.clj
;; Original:
(fn connected? [edges]
  (letfn [(group [sets edge]
            (let [gt-cntns (fn [sets node]
                             (reduce #(if (contains? %2 node) %2 %1)
                                     nil
                                     sets))
                  contains-left (gt-cntns sets (first edge))
                  contains-right (gt-cntns sets (last edge))]
              (cond
                (and (set? contains-left) (set? contains-right))
                  (conj (disj sets contains-left contains-right)
                        (clojure.set/union contains-left contains-right))
                (set? contains-left) 
                  (conj (disj sets contains-left)
                        (apply conj contains-left edge))
                (set? contains-right) 
                  (conj (disj sets contains-right)
                        (apply conj contains-right edge))
                :else
                  (conj sets (apply conj #{} edge)))))]
    (= 1 (count (reduce group #{} edges)))))
;;   So, the first thing to note here is that in 89 I did a simple BFS which
;; basically solves this problem. That said, let's examine this.
;;   What the algorithm does is it basically groups edges which share nodes
;; together into sets. Once the sets have been composed from all edges, count
;; the sets. If there's one set, it's connected, and if there are more sets,
;; then it's not connected.
;;   It's an extension of the initial concept I used in 89. For some reason,
;; observing my notes while I was working on 91, I specifically noted I didn't
;; want to do DFS/BFS, which, well, I guess this works, then. The grouping code
;; is a reduction function taking an edge and a set of sets (each set inside the
;; set represents a group of nodes), and is as follows:
(group [sets edge]
  (let [gt-cntns (fn [sets node]
                   (reduce #(if (contains? %2 node) %2 %1)
                           nil
                           sets))
        contains-left (gt-cntns sets (first edge))
        contains-right (gt-cntns sets (last edge))]
    (cond
      (and (set? contains-left) (set? contains-right))
        (conj (disj sets contains-left contains-right)
              (clojure.set/union contains-left contains-right))
      (set? contains-left) 
        (conj (disj sets contains-left)
              (apply conj contains-left edge))
      (set? contains-right) 
        (conj (disj sets contains-right)
              (apply conj contains-right edge))
      :else
        (conj sets (apply conj #{} edge)))))

;;   First, let's let at gt-cntns. This stands for "get-contains", and it returns
;; the last set which contains the specified node. This reduce is...actually a
;; filter, but the result is pulled into one value instead of a collection, like
;; filter would do. So, we can rewrite this by replacing it with the following:
gt-cntns (fn [sets node]
           (last (filter #(contains? % node) sets)))
;;   While we technically are taking the "last" set which contains the node, the
;; constraints of our problem are such that there is one, and only one, set
;; which contains any given node, because if a node is contained in two sets
;; there's an edge connecting them, and they're merged into once. Hence, the
;; last simply moves down a level - or causes it to become nil if it's empty.
;;   Given this, we can rewrite the let statement as the following:
(group [sets [left-node right-node :as edge]]
  (let [contains-left (last (filter #(contains? % left-node) sets))
        contains-right (last (filter #(contains? % right-node) sets))]
    ...

;;   The rest of the function is a series of conditions. There are four possible
;; situations, depending on the sets and the edge:
;;   * The edge bridges two sets - edge [:a :c] and sets #{#{:a :b} {:c :d}}
;;       In this situation, the two sets should be collapsed into one:
;;         #{:a :b :c :d}
;;   * The edge touches one set - edge [:a :c] and sets #{#{:a :b} #{:d :e}}
;;       In this situation, the touched set should be extended:
;;         #{#{:a :b :c} #{:d :e}}
;;   * The edge touches no sets - edge [:a :c] and set #{#{:d :e :f}}
;;       In this situation, the edge should form its own set:
;;         #{#{:a :c} #{:d :e :f}}
;;   With that in mind, here's the code:
...
(cond
  (and (set? contains-left) (set? contains-right))
    (conj (disj sets contains-left contains-right)
          (clojure.set/union contains-left contains-right))
  (set? contains-left) 
    (conj (disj sets contains-left)
          (apply conj contains-left edge))
  (set? contains-right) 
    (conj (disj sets contains-right)
          (apply conj contains-right edge))
  :else
    (conj sets (apply conj #{} edge)))))
;;   There's a bit of an awkward "disj and then rejoin" syntax going on here.
;; Unfortunately I honestly have no idea how I could clean that up into
;; something better; if it were a map or associative structure I could use
;; something like update-in, but sets? I'm not sure. That aside, it's a fairly
;; easy implementation of the above algorithm. It uses set? to check the
;; membership of the edges, then joins the edge to the appropriate set(s), or
;; inserts it as its own set.
;;   That (apply conj #{} edge) could really be a (into #{} edge) though.

;;   Our final code, using the grouping algorithm:
(fn connected? [edges]
  (letfn [(group [sets [left-node right-node :as edge]]
            (let [contains-left (last (filter #(contains? % left-node) sets))
                  contains-right (last (filter #(contains? % right-node) sets))]
              (cond
                (and (set? contains-left) (set? contains-right))
                  (conj (disj sets contains-left contains-right)
                        (clojure.set/union contains-left contains-right))
                (set? contains-left) 
                  (conj (disj sets contains-left)
                        (apply conj contains-left edge))
                (set? contains-right) 
                  (conj (disj sets contains-right)
                        (apply conj contains-right edge))
                :else
                  (conj sets (into #{} edge)))))]
    (= 1 (count (reduce group #{} edges)))))

;;   Of course, we could always just use a DFS, like we did in 89 - and it ends
;; up quite smaller, which is nice! Unfortunately that mapcat and reduce don't
;; really do it any favors in terms of readability...
(fn connected? [edges]
  (letfn [(bfs [graph nodes visited]
            (if (empty? nodes) visited
              (let [new-visited (apply conj (into #{} visited) nodes)
                    new-nodes (remove new-visited (mapcat #(graph %) nodes))]
                (recur graph new-nodes new-visited))))]
    (= (set (flatten (into [] edges)))
       (bfs (->> (mapcat (fn [[f s]] [[f s] [s f]]) edges)
                 (reduce (fn [m [k v]] (assoc m k (conj (get m k []) v))) {}))
            [(ffirst edges)]
            []))))