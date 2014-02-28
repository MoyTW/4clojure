;;;; 147 - Veitch, Please!
(fn __ [funcs]
  (let [conversion {'A [0 \1] 'a [0 \0] 'B [1 \1] 'b [1 \0] 'C [2 \1] 
                    'c [2 \0] 'D [3 \1] 'd [3 \0]}
        func-to-string
          (fn func-to-string [coll]
            (apply str (map second (sort-by first (map conversion coll)))))
        to-funcmap
          (fn to-funcmap [coll]
            (apply merge (map-indexed #(hash-map [%1] %2) 
                         (sort (map func-to-string coll)))))
        num-ones
          (fn num-ones [coll]
            (count (filter #{\1} coll)))
        diff-by-one
          (fn diff-by-one [left right]
            (= 1 (count (filter false? (map = left right)))))
        produce-diff
          (fn produce-diff [left right]
            (map #(if (not= %1 %2) \- %1) left right))
        merge-terms
          (fn merge-terms [n state]
            (let [less (state n)
                  more (state (inc n))]
              (for [[l-terms l-func] less
                    [m-terms m-func] more
                    :when (diff-by-one l-func m-func)]
                 [(concat l-terms m-terms) (produce-diff l-func m-func)])))
        find-unmerged
          (fn find-unmerged [old-state new-state]
            (let [old-minis (map (comp set first) (mapcat second old-state))
                  new-minis (map (comp set first) (mapcat second new-state))
                  new-map (apply merge (map #(apply hash-map %) (map #(vector (into #{} (first %)) (second %)) (mapcat second old-state)))) ; yeah this is pretttty dumb
                  unmerged (for [l-term old-minis
                                 :when (every? false? (map #(clojure.set/subset? l-term %) new-minis))]
                              [l-term (new-map l-term)])]
              (into #{} unmerged)))
        step
          (fn step [state final]
            (let [r (butlast (sort (keys state)))
                  next-state (group-by #(num-ones (second %)) 
                                       (mapcat #(merge-terms % state) r))
                  unmerged (find-unmerged state next-state)]
              (if (seq next-state)
                  (recur next-state (concat final unmerged))
                  (concat final unmerged))))
        get-essential
          (fn get-essential [mini unmerged]
            (let [covers (filter #(contains? (first %1) mini) unmerged)]
              (if (= 1 (count covers))
                  [(second (first covers))]
                  nil)))
        find-essentials
          (fn find-essentials [miniterms unmerged]
            (mapcat #(get-essential % unmerged) miniterms))
        to-set
          (fn to-set [coll]
            (into #{} (map (clojure.set/map-invert conversion)
                           (filter #(not= (second %) \-) (map-indexed vector coll)))))
        to-sets
          (fn to-sets [coll]
            (into #{} (map to-set coll)))]
    (let [func-map (to-funcmap funcs)
          miniterms (apply concat (keys func-map))
          implicants (step (group-by #(num-ones (second %)) func-map) #{})]
    (to-sets (find-essentials miniterms implicants)))))