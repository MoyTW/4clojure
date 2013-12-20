;;;; -----=====***** 105 (11 lines, 11 total) *****=====-----
(fn build-map [kvs]
  (let [red-func
          (fn [[mp lst] nxt]
            (cond
              (and (keyword? lst) (keyword? nxt)) [(assoc mp lst []) nxt]
              (keyword? lst) [(assoc mp lst nxt) nxt]
              :else [mp nxt]))
        grouped-vals
          (mapcat #(if (keyword? (first %)) % [%])
                  (partition-by keyword? kvs))]
    (first (reduce red-func [{} nil] grouped-vals))))

;;;; -----=====***** 106 (10 lines, 21 total) *****=====-----
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

;;;; -----=====***** 107 (2 lines, 23 total) *****=====-----
(fn build-func [n]
  (fn f-of-x [x] (int (Math/pow x n))))

;;;; -----=====***** 108 (7 lines, 30 total) *****=====-----
(fn lazy-search [& seqs]
  (if (= 1 (count seqs)) (ffirst seqs)
    (loop [seqs seqs]
      (let [firstvals (map first seqs)
            low-val (apply min firstvals)]
        (if (apply = firstvals) low-val
          (recur (map #(if (= low-val (first %)) (rest %) %) seqs)))))))

;;;; -----=====***** 109 (0 lines, 30 total) *****=====-----
;; No such problem!

;;;; -----=====***** 110 (5 lines, 35 total) *****=====-----
(fn weird [coll]
  (letfn [(step [coll]
           (mapcat #(list (count %) (first %)) 
                   (partition-by (fn [x] x) coll)))]
    (rest (iterate step coll))))

;;;; -----=====***** 111 (16 lines, 51 total) *****=====-----	
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