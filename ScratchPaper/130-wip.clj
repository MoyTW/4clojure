;; Hey it's a tree.
;; Okay, so
;; Algorithm: Leapfrog, put top below on right

;; By hand - pull d
;       a  
;   /---+--\
;   b       f
;  /|\     / \
; c d e   g   h

;     a
;    / \
;   d   f
;   |  / \
;   b  g  h
;  / \
; c   e

;     d
;    / \
;   b   a
;  /\   |
; c  e  f
;      / \
;     g   h

;; No that's wrong.

;; Okay hold on, where does the d go?
;; d = b, b > a; therefore d > a, yes? ALSO b > a.
;; Why does a end up attached to the b instead of the d, in the 'correct' one? OH, because it has to keep the connections intact...
;; hhhhmmmmmm

;; so, we pull up our target node - and then reorient tree
;;    a
;;  b    f
;; c d e g h
;; PULL UP d
;; ->
;;  d
;;  +-----a
;;  b     f
;; c e   g h
;; Push down a
;;     d
;;     b
;;   c e a
;;       f
;;      g h

;; That's not...terribly elegant, but I think it'll work.
;; Transform nodes into maps:
;; {:value _, :parents [], :children []}
(def fifth 
  {'a {:parents [], :children ['b 'f]},
   'b {:parents ['a], :children ['c 'd 'e]},
   'c {:parents ['b], :children []},
   'd {:parents ['b], :children []},
   'e {:parents ['b], :children []},
   'f {:parents ['a], :children ['g 'h]},
   'g {:parents ['f], :children []},
   'h {:parents ['f], :children []}})

;; PULL UP d
;; find the parent of d
(def parent-of-d (first ((fifth 'd) :parents)))
(assoc (fifth parent-of-d) :children (remove #{'d} ((fifth parent-of-d) :children)))
(defn swap-positions [graph target]
  (let [parent-node (first ((graph target) :parents))
        modified-parent (assoc (graph parent-node)
                               :parents (conj ((graph parent-node) :parents) target)
                               :children (remove #{target} ((graph parent-node) :children)))
        modified-target (assoc (graph target)
                               :parents []
                               :children (conj ((graph target) :children) parent-node))]
    (assoc graph parent-node modified-parent target modified-target)))
(swap-positions fifth 'd)

;; results in:
{a {:parents [], :children [b f]},
 b {:children (c e), :parents [a d]},
 c {:parents [b], :children []},
 d {:parents [], :children [b]},
 e {:parents [b], :children []},
 f {:children [g h], :parents [a]},
 g {:parents [f], :children []},
 h {:parents [f], :children []}}
 
;; Now we need to push down a
(def swapped (swap-positions fifth 'd))
(filter #(> (count ((second %) :parents)) 1) swapped)
;; Okay how do we know which of the two parents to push down? uuuuhm...that's a good question, actually.

;; Should put in proper order!
;; how do we do ordering? well if it's a sorted btree that
;; uh, no, it's not a sorted btree, there are straight connections (a -> b -> c). Hmm. Well...how about applying an ordering by walking left-to-right, and then enforcing it?
;; This is getting increasingly convoluted.
;; Okay but wait, since it's not a binary tree...how does the ordering work? For example, if we have the tree:
;;     a
;;     b
;;    c d
;; and we want to reparent c to the top, we have two possible trees:
;;     c    c
;;     b    b
;;    a d  d a
;; Which of these is the correct tree?
;; you know what screw it we'll just assome that we put the tree on the right because that will solve this problem

(defn swap-positions [graph target]
  (let [parent-node (first ((graph target) :parents))
        modified-parent (assoc (graph parent-node)
                               :parents (cons target ((graph parent-node) :parents))
                               :children (remove #{target} ((graph parent-node) :children)))
        modified-target (assoc (graph target)
                               :parents []
                               :children (conj ((graph target) :children) parent-node))]
    (assoc graph parent-node modified-parent target modified-target)))
(swap-positions fifth 'd)
;; Returns:
{a {:parents [], :children [b f]},
 b {:children (c e), :parents (d a)},
 c {:parents [b], :children []},
 d {:parents [], :children [b]},
 e {:parents [b], :children []},
 f {:children [g h], :parents [a]},
 g {:parents [f], :children []},
 h {:parents [f], :children []}}

(def swapped (swap-positions fifth 'd))
(filter #(> (count ((second %) :parents)) 1) swapped)

(defn pull-down [graph]
  (let [multiparent (first (filter #(> (count ((second %) :parents)) 1) graph))
        push-node-key (second (:parents (second multiparent)))
        push-node (graph push-node-key)
        modified-multiparent 
          (assoc (graph (first multiparent))
                 :parents [(first ((graph (first multiparent)) :parents))]
                 :children (conj (into [] ((graph (first multiparent)) :children)) 
                                 push-node-key))
        modified-push-node
          (assoc push-node
                 :parents [(first multiparent)]
                 :children (remove #{(first multiparent)} (push-node :children)))]
    ;;modified-push-node))
    (assoc graph (first multiparent) modified-multiparent push-node-key modified-push-node)))

(pull-down swapped)
;; Yeah this only works on one node, though we could convert it fairly easily if needed.
;; Results:
{a {:parents [b], :children (f)},
 b {:children [c e a], :parents [d]},
 c {:parents [b], :children []},
 d {:parents [], :children [b]},
 e {:parents [b], :children []},
 f {:children [g h], :parents [a]},
 g {:parents [f], :children []},
 h {:parents [f], :children []}}
;; Convert to nested list representation:

(ffirst (filter #(empty? ((second %) :parents)) (pull-down swapped))) ; head node key
(defn to-list [m key]
  (let [{children :children} (m key)]
    (if (seq children)
        (cons key (map #(to-list m %) children))
        [key])))
(to-list (pull-down swapped) 
         (ffirst (filter #(empty? ((second %) :parents)) (pull-down swapped))))

(def endmap (pull-down swapped))
;; aaand we can put that in a function
;[head (ffirst (filter #(empty? ((second %) :parents)) m))
(defn to-list [m]
  (letfn [(convert-node [key]
            (let [{children :children} (m key)]
              (if (seq children)
                  (cons key (map #(convert-node %) children))
                  [key])))]
    (convert-node (ffirst (filter #(empty? ((second %) :parents)) m)))))
(to-list endmap)

(def fifth-list '(a (b (c) (d) (e)) (f (g) (h))))

;; '(f (g) (h)) ->
;; {f {:parents [], :children [g h]}
;;  g {:parents [f], :children []}
;;  h {:parents [f], :children []}}

(defn to-map-step [parent [key & children] m]
  (let [node [key {:parents (if parent [parent] [])
                   :children (map first children)}]]
    (if (seq children)
        (apply merge (conj m node) (map #(to-map-step key % m) children))
        (conj m node))))
(to-map-step nil '(f (g) (h)) {})
(to-map-step 'f '(g) {})
(to-map-step nil fifth-list {})
(= fifth (to-map-step nil fifth-list {}))
;; ugh so ugly

;; what if it is already a line?
(defn pull-down [graph]
  (if-let [multiparent (first (filter #(> (count ((second %) :parents)) 1) graph))]
    (let [push-node-key (second (:parents (second multiparent)))
          push-node (graph push-node-key)
          modified-multiparent 
            (assoc (graph (first multiparent))
                   :parents [(first ((graph (first multiparent)) :parents))]
                   :children (conj (into [] ((graph (first multiparent)) :children)) 
                                   push-node-key))
          modified-push-node
            (assoc push-node
                   :parents [(first multiparent)]
                   :children (remove #{(first multiparent)} (push-node :children)))]
      ;;modified-push-node))
      (assoc graph (first multiparent) modified-multiparent push-node-key modified-push-node))
    graph))

(pull-down (swap-positions (to-map-step nil '(t (e) (a)) {}) 'a))