;; 4Clojure Problem 111. Crossword puzzle
;; url: http://www.4clojure.com/problem/111
(fn filter-possible-matches [word coll]
  (let [filter-func
          (fn [space]
            (and (= (count space) (count word))
                 (every? #(or (= (first %) (second %)) (= (second %) \_)) 
                         (apply map vector [word space]))))
        hori
          (mapcat 
            #(clojure.string/split (clojure.string/replace % #"\s" "") #"#")
            coll)
        vert 
          (mapcat 
            #(clojure.string/split % #"#") 
            (apply map str (map #(clojure.string/replace % #"\s" "") coll)))
        pmatches (filter filter-func (concat hori vert))]
    (boolean (seq pmatches))))