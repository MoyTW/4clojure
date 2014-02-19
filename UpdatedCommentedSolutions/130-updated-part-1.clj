;;;; 130 - Tree reparenting
;;; Scratch: https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/130-wip.clj
;; Original:
(fn bah [target tree]
  (letfn [(to-map-step [parent [key & children] m]
            (let [node [key {:parents (if parent [parent] [])
                             :children (map first children)}]]
              (if (seq children)
                  (apply merge (conj m node) (map #(to-map-step key % m) children))
                  (conj m node))))
          (swap-positions [target graph]
            (if (empty? ((graph target) :parents)) 
                graph
                (let [parent-node (first ((graph target) :parents))
                      modified-parent (assoc (graph parent-node)
                                             :parents (cons target ((graph parent-node) :parents))
                                             :children (remove #{target} ((graph parent-node) :children)))
                      modified-target (assoc (graph target)
                                             :parents []
                                             :children (conj (into [] ((graph target) :children)) parent-node))]
                  (assoc graph parent-node modified-parent target modified-target))))
          (pull-down [graph]
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
                (assoc graph (first multiparent) modified-multiparent push-node-key modified-push-node))
              graph))
          (to-list [m]
            (letfn [(convert-node [key]
                      (let [{children :children} (m key)]
                        (if (seq children)
                            (cons key (map #(convert-node %) children))
                            [key])))]
              (convert-node (ffirst (filter #(empty? ((second %) :parents)) m)))))]
    (->> (to-map-step nil tree {}) 
         (swap-positions target)
         (pull-down)
         (to-list))))
;; This one was a real doozy.
;; The way in which I chose to solve it was...inelegant. It works with a different internal representation than the given representation - instead of a nested list of lists, it uses a map of nodes, like so:
;;   {a {:parents [], :children [b f]},
;;    b {:children (c e), :parents [a d]},
;;    c {:parents [b], :children []},
;;    d {:parents [], :children [b]},
;;    e {:parents [b], :children []},
;;    f {:children [g h], :parents [a]},
;;    g {:parents [f], :children []},
;;    h {:parents [f], :children []}}
;; When it processes the list, what it does is finds the node that it wants to pull up, clears out the :parents element of that node, and then adds to its :children node the former parent. The former parent on the other hand has the target node added to its :parents element.
;; Then, in the second pass, the multi-parented node 'pushes' its new parent downwards, to become a child of the node, again by screwing around with the :parents and :children elements of the two.
;;   Finally, it converts the nodes back to a list representation.
;;   Wow, that's convoluted. Okay, well, let's ignore the code to convert to and from the tree and concentrate on the actual manipulations. Specifically, let's look at swap-positions and push-node.
;; One thing to note is that we can actually merge these two functions. The "Find the thing with multiple parents" code will always turn up the node which was formerly the parent of the node we want to pull up to the top, so we can actually do away with that and directly resolve all three modifications in one step. Well, sort of - it's complicated by the fact that sometimes pulling up the targeted node won't lead to a multiparent situation, so we have to handle that, too. As a first pass:
(swap-positions [target-key graph]
  (if (empty? ((graph target-key) :parents)) 
      graph
      (let [parent-key (first ((graph target-key) :parents))
            push-node-key (first ((graph parent-key) :parents))
            modified-parent (assoc (graph parent-key)
                                   :parents [target-key]
                                   :children (concat (vec (remove #{target-key} ((graph parent-key) :children))) 
                                                     (if push-node-key [push-node-key])))
            modified-target-key (assoc (graph target-key)
                                   :parents []
                                   :children (conj (into [] ((graph target-key) :children)) parent-key))
            pulled-graph (assoc graph parent-key modified-parent target-key modified-target-key)]
        (if push-node-key
            (assoc pulled-graph
                   push-node-key (assoc (graph push-node-key)
                                        :parents [parent-key]
                                        :children (remove #{parent-key} ((graph push-node-key) :children))))
            pulled-graph))))
;; So, what's going on here is I merged the two functions. Now it always modifies the parent and the target to swap, and if there would be a node which would be multiparented, it handles the reparenting of that node in the (if push-node-key ...). It's very ugly.
(swap-positions [target-key graph]
  (if (empty? ((graph target-key) :parents)) 
      graph
      (let [parent-key (first ((graph target-key) :parents))
            push-node-key (first ((graph parent-key) :parents))
            modified-parent (assoc (graph parent-key)
                                   :parents [target-key]
                                   :children (concat (vec (remove #{target-key} ((graph parent-key) :children))) 
                                                     (if push-node-key [push-node-key])))
            modified-target-key (assoc (graph target-key)
                                   :parents []
                                   :children (conj (into [] ((graph target-key) :children)) parent-key))
            pulled-graph (assoc graph parent-key modified-parent target-key modified-target-key)]
        (if push-node-key
            (assoc pulled-graph
                   push-node-key (assoc (graph push-node-key)
                                        :parents [parent-key]
                                        :children (remove #{parent-key} ((graph push-node-key) :children))))
            pulled-graph))))
;; I especially don't like that awkward (if push-node-key [push-node-key]) structure when I'm building the new parent. The thing is, it either goes there or we have to make the modification in the if proper, which - actually, hold on, let me see what that looks like with update-in:
(swap-positions [target-key graph]
            (if (empty? ((graph target-key) :parents)) 
                graph
                (let [parent-key (first ((graph target-key) :parents))
                      push-node-key (first ((graph parent-key) :parents))
                      modified-parent (assoc (graph parent-key)
                                             :parents [target-key]
                                             :children (vec (remove #{target-key} ((graph parent-key) :children))))
                      modified-target-key (assoc (graph target-key)
                                             :parents []
                                             :children (conj (into [] ((graph target-key) :children)) parent-key))
                      pulled-graph (assoc graph parent-key modified-parent target-key modified-target-key)]
                  (if push-node-key
                      (-> pulled-graph
                          (assoc push-node-key (assoc (graph push-node-key)
                                                      :parents [parent-key]
                                                      :children (remove #{parent-key} 
                                                                        ((graph push-node-key) :children))))
                          (update-in [parent-key :children] #(conj % push-node-key)))
                      pulled-graph))))
;; Uh, hold on why am I doing this assoc thing when - wait, what. Those...those don't need to be assoc'd at all. I'm replacing them wholesale. Talking about modified-parent and modified-target, hold on:
(swap-positions [target-key graph]
  (if (empty? ((graph target-key) :parents)) 
      graph
      (let [parent-key (first ((graph target-key) :parents))
            push-node-key (first ((graph parent-key) :parents))
            modified-parent {:parents [target-key]
                             :children (vec (remove #{target-key} ((graph parent-key) :children)))}
            modified-target-key {:parents []
                                 :children (conj (vec ((graph target-key) :children)) parent-key)}
            pulled-graph (assoc graph parent-key modified-parent target-key modified-target-key)]
        (if push-node-key
            (-> pulled-graph
                (assoc push-node-key 
                       {:parents [parent-key]
                        :children (remove #{parent-key} ((graph push-node-key) :children))})
                (update-in [parent-key :children] #(conj % push-node-key)))
            pulled-graph))))
;; It won't be winning any beauty contests, that's for sure. I considered an if-let, but then I'd just have to put a let anyways because for whatever arcane reason if-let allows one and only one binding, so that wouldn't get me any further prettiness. Still, this is a clunky way to solve the problem - converting it into a completely, utterly different internal representatnion, then converting it back at the end! Before we explore alternatives, let's plug this into the main body and count it up:
(fn bah [target tree]
  (letfn [(to-map-step [parent [key & children] m]
            (let [node [key {:parents (if parent [parent] [])
                             :children (map first children)}]]
              (if (seq children)
                  (apply merge (conj m node) (map #(to-map-step key % m) children))
                  (conj m node))))
          (swap-positions [target-key graph]
            (if (empty? ((graph target-key) :parents)) 
                graph
                (let [parent-key (first ((graph target-key) :parents))
                      push-node-key (first ((graph parent-key) :parents))
                      modified-parent {:parents [target-key]
                                       :children (vec (remove #{target-key} ((graph parent-key) :children)))}
                      modified-target-key {:parents []
                                           :children (conj (vec ((graph target-key) :children)) parent-key)}
                      pulled-graph (assoc graph parent-key modified-parent target-key modified-target-key)]
                  (if push-node-key
                      (-> pulled-graph
                          (assoc push-node-key 
                                 {:parents [parent-key]
                                  :children (remove #{parent-key} ((graph push-node-key) :children))})
                          (update-in [parent-key :children] #(conj % push-node-key)))
                      pulled-graph))))
          (to-list [m]
            (letfn [(convert-node [key]
                      (let [{children :children} (m key)]
                        (if (seq children)
                            (cons key (map #(convert-node %) children))
                            [key])))]
              (convert-node (ffirst (filter #(empty? ((second %) :parents)) m)))))]
    (->> (to-map-step nil tree {}) 
         (swap-positions target)
         (to-list))))
;; It's down to 34 lines, and in my opinion is...somewhat more readable. It's still not really readable, but you know, it's a step up from total incomprehensibility. It's also like 25% shorter!
;; Aaaand, oh boy, I just looked at some of the solutions available online and I couldn't make heads nor tails of them on first inspection! So, here's what I think I'll do. I'll put this up as part 1, and then later, I'll come back and work my way through the solutions available online, see if I can't pick them apart, and compare them to my terrible, terrible solution.