;; 4Clojure Problem 89. Graph Tour
;; url: http://www.4clojure.com/problem/89
(fn edge-tour? [edge-vec]
  (letfn [(connected? [edges]
            (letfn [(bfs [graph nodes visited]
                      (if (empty? nodes) visited
                        (let [new-visited (apply conj (into #{} visited) nodes)
                              new-nodes (remove new-visited (mapcat #(graph %) nodes))]
                          (recur graph new-nodes new-visited))))]
              (= (set (flatten edges))
                 (bfs (->> (mapcat (fn [[f s]] [[f s] [s f]]) edges)
                           (reduce (fn [m [k v]] (assoc m k (conj (get m k []) v))) {}))
                      [(ffirst edges)]
                      []))))]
    (if-not (connected? edge-vec) false
      (->> (frequencies (flatten edge-vec))
           (filter #(odd? (last %)))
           (count)
           (>= 2)))))