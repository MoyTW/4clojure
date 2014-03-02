(fn __ [coll]
  (let [candidates
          (fn candidates [size array]
            (let [height (count array)
                  height-range (range (- (inc height) size))
                  width (count (first array))
                  width-range (range (- (inc width) size))]
              (for [row height-range
                    col width-range
                    :let [segment (map #(take size (drop col %))
                                       (take size (drop row array)))]
                    :when (not-any? #(= nil %) (flatten segment))]
                segment)))
        uniques?
          (fn uniques? [array] 
            (= (count array) (count (distinct (flatten array)))))
        latin?
          (fn latin? [square]
            (if (uniques? square)
                (and (every? uniques? square)
                     (every? uniques? (apply map vector square)))))
        merge-rows
          (fn merge-rows [out in]
            (for [o out
                  i in]
              (conj o i)))
        gen-shifts
          (fn gen-shifts [width coll]
            (let [pad (- width (count coll))
                  do-shift #(conj (vec (rest %)) nil)]
              (cond
                (zero? pad) [coll]
                (empty? coll) [(vec (repeat pad nil))]
                :else
                  (take (inc pad) 
                        (iterate do-shift 
                                 (vec (apply conj (seq coll) 
                                                  (repeat pad nil))))))))
        square-latin
          (fn square-latin [n coll]
            (filter latin? (mapcat #(candidates n %) coll)))
        get-latin
          (fn get-latin [coll]
            (let [m (first (sort > (map count coll)))
                  array (reduce merge-rows [[]] (map #(gen-shifts m %) coll))
                  sizes (range 2 (inc (min (count coll) m)))]
              (mapcat #(square-latin % array) sizes)))
        squares (distinct (get-latin coll))]
    (into {}
          (for [[k v] (group-by count squares)]
            [k (count v)]))))