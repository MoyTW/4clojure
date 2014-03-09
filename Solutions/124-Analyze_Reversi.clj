;; 4Clojure Problem 124. Analyze Reversi
;; url: http://www.4clojure.com/problem/124
(fn analyze-reversi [board piece]
  (let [other-piece (if (= piece 'w) 'b 'w)
        dir-vecs (letfn [(id-func [i & args] i)]
                   (for [r [+ - id-func]
                         c [+ - id-func]
                         :when (not= [id-func id-func] [r c])]
                     [r c]))
        matching-coordinates (for [r (range (count board))
                                   c (range (count (first board)))
                                   :when (= piece (get-in board [r c]))]
                               [r c])
        line-from (fn [start-coords [f-r f-c]]
                    (map #(vector % (get-in board %))
                         (take-while #(get-in board %)
                                     (reductions
                                       (fn f [[r c] n]
                                         [(f-r r n) (f-c c n)])
                                       start-coords
                                       (repeat 1)))))
        placement (fn [line]
                    (let [[other [first-not-other & rest-not-other]] (split-with #(= other-piece (last %)) (rest line))]
                      (if (and (> (count line) 2) (> (count other) 0) (= 'e (last first-not-other)))
                          {(first first-not-other) (into #{} (map #(first %) other))}
                          nil)))
        ]
    (apply merge (filter map?
                         (for [coords matching-coordinates
                               dv dir-vecs]
                           (placement (line-from coords dv)))))))