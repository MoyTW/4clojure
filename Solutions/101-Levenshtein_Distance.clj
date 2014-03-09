;; 4Clojure Problem 101. Levenshtein Distance
;; url: http://www.4clojure.com/problem/101
(fn l-d [left right]
  (let [cache (atom {})]
    (letfn [(lev-dist [left right]
              (let [cost (if (= (last left) (last right)) 0 1)
                    key [(count left) (count right)]]
                (cond 
                  (= 0 (count left)) 
                      (do (swap! cache assoc key (count right)) (count right))
                  (= 0 (count right)) 
                      (do (swap! cache assoc key (count left)) (count left))
                  (contains? @cache key) (get @cache key)
                  :else 
                  (let [val 
                          (min 
                            (inc (get @cache key (lev-dist (butlast left) right)))
                            (inc (get @cache key (lev-dist left (butlast right))))
                            (+ cost (get @cache key (lev-dist (butlast left) (butlast right)))))]
                    (do (swap! cache assoc key val) val)))))]
      (lev-dist left right))))