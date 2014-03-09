;; 4Clojure Problem 117. For Science!
;; url: http://www.4clojure.com/problem/117
(fn for-science [coll]
  (letfn [(mark-next [coll]
            (->> (conj coll \#)
                 (reduce (fn [[out p c] n]
                           (if (and (contains? #{\space \C} c) (or (= p \F) (= n \F)))
                               [(conj out \N) \N n]
                               [(conj out c) c n]))
                          [[] nil nil])
                  ((comp rest first))))
          (swap-chars [coll]
            (map (fn [c]
                   (cond
                     (= c \N) \F
                     (= c \F) \#
                     :else c))
                 coll))
          (all-next [coll]
            (->> (into [] coll)
                 (map mark-next)
                 (apply map vector)
                 (map mark-next)
                 (map swap-chars)
                 (apply map vector)))]
    (loop [maze (map #(into [] (clojure.string/replace % #"M" "F")) coll)]
      (let [flat-maze (flatten maze)]
        (cond
          (not-any? #{\C} flat-maze) true
          (not-any? #{\F} flat-maze) false
          :else (recur (all-next maze)))))))