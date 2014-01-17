;;;; -----=====***** 116 (11 lines, 11 total) *****=====-----
(fn is-balanced-prime [n]
  (let [previousProbablePrime 
          (fn [n]
            (loop [n (dec n)]
              (if (.isProbablePrime (biginteger n) 500)
                n
                (recur (dec n)))))
        n-prime (.nextProbablePrime (biginteger n))
        p-prime (previousProbablePrime n)]
    (and (.isProbablePrime (biginteger n) 500) 
         (= n (/ (+ n-prime p-prime) 2)))))

;;;; -----=====***** 117 (29 lines, 40 total) *****=====-----
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

;;;; -----=====***** 118 (6 lines, 46 total) *****=====-----
(fn my-map [pred coll]
  (let [step (fn [p c]
                 (when-let [s (seq c)] ; when (seq c) is true, let s = (seq c), else return nil
                   (cons (pred (first s)) 
                         (my-map p (rest s)))))]
    (lazy-seq (step pred coll))))