;; 4Clojure Problem 106. Number Maze
;; url: http://www.4clojure.com/problem/106
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