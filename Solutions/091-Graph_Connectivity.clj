;; 4Clojure Problem 91. Graph Connectivity
;; url: http://www.4clojure.com/problem/91
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
                    (set? contains-left) (conj (disj sets contains-left) 
                                               (apply conj contains-left edge))
                    (set? contains-right) (conj (disj sets contains-right) 
                                                (apply conj contains-right edge))
                    :else
                    (conj sets (apply conj #{} edge)))))]
    (= 1 (count (reduce group #{} edges)))))