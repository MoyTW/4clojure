;; 4Clojure Problem 131. Sum Some Set Subsets
;; url: http://www.4clojure.com/problem/131
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