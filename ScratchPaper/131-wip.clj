;; The phrasing on that was a little confusing, but basically it means "Expand each set into all possible subsets, and sum them. If any of the resulting sums are shared across the input set, return true, otherwise return false."
;; Well that's more an algorithm for solving it than the problem description, really, but...
;; Anyways, how do we generate all sets from the set? Wait, we...totally did that, didn't we.
;; Yup, from #85:
(defn power-set [trgt-st]
  (letfn [(break-up [s] (map #(disj s %) s))
          (next-line [s] (set (mapcat break-up s)))]
    (loop [st #{trgt-st} pwr-st #{trgt-st}]
      (if (= st #{#{}}) pwr-st
        (recur (next-line st) (apply conj pwr-st (next-line st)))))))

        
(defn __ [& args]
  (let [[first-sum & rest-sums] (->> (map power-set args)
                                     (map #(remove empty? %))
                                     (map (fn [s] (map #(apply + %) s))) ; pretty ugly
                                     (map #(into #{} %)))]
    (if-let [result (some (fn [v] (every? #(contains? % v) rest-sums)) first-sum)]
      result
      false)))

(__ #{-1 1 99} 
    #{-2 2 888}
    #{-3 3 7777})
        
(= true  (__ #{-1 1 99} 
             #{-2 2 888}
             #{-3 3 7777}))
(= false (__ #{1}
             #{2}
             #{3}
             #{4})) 
(= true  (__ #{1}))
(= false (__ #{1 -3 51 9} 
             #{0} 
             #{9 2 81 33}))
(= true  (__ #{1 3 5}
             #{9 11 4}
             #{-3 12 3}
             #{-3 4 -2 10}))
(= false (__ #{-1 -2 -3 -4 -5 -6}
             #{1 2 3 4 5 6 7 8 9}))
(= true  (__ #{1 3 5 7}
             #{2 4 6 8}))
(= true  (__ #{-1 3 -5 7 -9 11 -13 15}
             #{1 -3 5 -7 9 -11 13 -15}
             #{1 -1 2 -2 4 -4 8 -8}))        
(= true  (__ #{-10 9 -8 7 -6 5 -4 3 -2 1}
             #{10 -9 8 -7 6 -5 4 -3 2 -1}))

             
(defn power-set [trgt-st]
  (letfn [(break-up [s] (map #(disj s %) s))
          (next-line [s] (set (mapcat break-up s)))]
    (loop [st #{trgt-st} pwr-st #{trgt-st}]
      (if (= st #{#{}}) pwr-st
        (recur (next-line st) (apply conj pwr-st (next-line st)))))))

        
(fn __ [& args]
  (letfn [(power-set [trgt-st]
            (letfn [(break-up [s] (map #(disj s %) s))
                    (next-line [s] (set (mapcat break-up s)))]
              (loop [st #{trgt-st} pwr-st #{trgt-st}]
                (if (= st #{#{}}) pwr-st
                  (recur (next-line st) (apply conj pwr-st (next-line st)))))))]
    (let [[first-sum & rest-sums] (->> (map power-set args)
                                       (map #(remove empty? %))
                                       (map (fn [s] (map #(apply + %) s))) ; pretty ugly
                                       (map #(into #{} %)))]
      (if-let [result (some (fn [v] (every? #(contains? % v) rest-sums)) first-sum)]
        result
        false))))