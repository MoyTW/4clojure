(defn mark-next [coll]
  (->> (conj coll \#)
       (reduce (fn [[out p c] n]
                 (if (and (contains? #{\space \C} c) (or (= p \F) (= n \F)))
                     [(conj out \N) \N n]
                     [(conj out c) c n]))
                [[] nil nil])
        ((comp rest first))))

(defn swap-chars [coll]
  (map (fn [c]
         (cond
           (= c \N) \F
           (= c \F) \#
           :else c))
       coll))

(defn all-next [coll]
  (->> (into [] coll)
       (map mark-next)
       (apply map vector)
       (map mark-next)
       (map swap-chars)
       (apply map vector)))

(defn for-science [coll]
  (loop [maze (map #(into [] (clojure.string/replace % #"M" "F")) coll)]
    (let [flat-maze (flatten maze)]
      (cond
        (not-any? #{\C} flat-maze) true
        (not-any? #{\F} flat-maze) false
        :else (recur (all-next maze))))))
        
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