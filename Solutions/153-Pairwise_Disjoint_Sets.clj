;;;; 153 - Pairwise Disjoint Sets (13 lines)
(fn __ [sets]
  (letfn [(disjoint? [left right]
            (zero? (count (for [l left
                                r right
                               :when (= l r)]
                            :shared))))]
    (empty?
      (apply concat
             (for [s sets
                   :let [other (filter #(not= s %) sets)]]
               (for [r other
                     :when (not (disjoint? s r))]
                 :covers))))))