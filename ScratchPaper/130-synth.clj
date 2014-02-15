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