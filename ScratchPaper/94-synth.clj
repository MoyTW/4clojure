; Okay.
(defn adj-coords [[x y]]
    (for [x-n [(dec x) x (inc x)]
          y-n [(dec y) y (inc y)]
          :when (not= [x-n y-n] [x y])]
      [x-n y-n]))
(defn count-adj [[x y] cells]
  (let [adj (adj-coords [x y])]
    (reduce #(if (contains? cells %2) (inc %1) %1) 0 adj)))
(defn lives? [[x y] cells]
  (let [num-live (count-adj [x y] cells)]
    (if (contains? cells [x y])
      (boolean (#{2 3} num-live))
      (boolean (#{3} num-live)))))
(defn step [cells x-max y-max]
  (for [x-n (take x-max (range))
        y-n (take y-max (range))
        :when (lives? [x-n y-n] cells)]
    [x-n y-n]))
(defn collapse-str [s y]
  (->> s
       (map #(if (= \# %2) [%1 y] nil) (take (count s) (range)))
       (filter #(not= % nil))
       (into #{})))
(defn collapse-vec [v]
  (->> v
       (map #(vector %1 %2) (reverse (take (count v) (range))))
       (reduce #(concat %1 (collapse-str (last %2) (first %2))) [])
       (into #{})))
(defn explode-set [s max-x max-y]
  (let [spaces-list (into [] (take max-x (repeat \space)))
        y-range (take max-y (range))
        live-map 
          (reduce (fn map-set-to-rows [mp st]
                    (assoc mp 
                      (last st)
                      (conj (get mp (last st)) (first st))))
                  (zipmap y-range (take max-y (repeat [])))
                  s)
        explode-str
          (fn [s-e]
            (->> s-e
                 (reduce #(assoc %1 %2 \#) spaces-list)
                 (apply str)))]
    (->> y-range
         (into [])
         (map #(explode-str (get live-map %)))
         (reverse))))

; ohgod
(fn life [board]
  (let [y-max (count board)
        x-max (count (first board))
        adj-coords
          (fn [[x y]]
            (for [x-n [(dec x) x (inc x)]
                  y-n [(dec y) y (inc y)]
                  :when (not= [x-n y-n] [x y])]
            [x-n y-n]))
        count-adj
          (fn [[x y] cells]
             (let [adj (adj-coords [x y])]
               (reduce #(if (contains? cells %2) (inc %1) %1) 0 adj)))
        lives?
          (fn [[x y] cells]
            (let [num-live (count-adj [x y] cells)]
              (if (contains? cells [x y])
                (boolean (#{2 3} num-live))
                (boolean (#{3} num-live)))))
        step 
          (fn [cells]
            (for [x-n (take x-max (range))
                  y-n (take y-max (range))
                  :when (lives? [x-n y-n] cells)]
              [x-n y-n]))
        collapse-str
          (fn [s y]
            (->> s
                 (map #(if (= \# %2) [%1 y] nil) (take (count s) (range)))
                 (filter #(not= % nil))
                 (into #{})))
        collapse-vec
          (fn [v]
            (->> v
                 (map #(vector %1 %2) (reverse (take (count v) (range))))
                 (reduce #(concat %1 (collapse-str (last %2) (first %2))) [])
                 (into #{})))
        explode-set
          (fn [s]
            (let [spaces-list (into [] (take x-max (repeat \space)))
                  y-range (take y-max (range))
                  live-map 
                    (reduce (fn map-set-to-rows [mp st]
                              (assoc mp 
                                (last st)
                                (conj (get mp (last st)) (first st))))
                            (zipmap y-range (take y-max (repeat [])))
                            s)
                  explode-str
                    (fn [s-e]
                      (->> s-e
                           (reduce #(assoc %1 %2 \#) spaces-list)
                           (apply str)))]
              (->> y-range
                   (into [])
                   (map #(explode-str (get live-map %)))
                   (reverse))))]
    (explode-set (step (collapse-vec board)))))