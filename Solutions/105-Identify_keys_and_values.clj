;; 4Clojure Problem 105. Identify keys and values
;; url: http://www.4clojure.com/problem/105
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