;; 4Clojure Problem 153. Pairwise Disjoint Sets
;; url: http://www.4clojure.com/problem/153
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