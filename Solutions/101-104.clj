;;;; -----=====***** 101 (19 lines, 19 total) *****=====-----
(fn l-d [left right]
  (let [cache (atom {})]
    (letfn [(lev-dist [left right]
              (let [cost (if (= (last left) (last right)) 0 1)
                    key [(count left) (count right)]]
                (cond 
                  (= 0 (count left)) 
                      (do (swap! cache assoc key (count right)) (count right))
                  (= 0 (count right)) 
                      (do (swap! cache assoc key (count left)) (count left))
                  (contains? @cache key) (get @cache key)
                  :else 
                  (let [val 
                          (min 
                            (inc (get @cache key (lev-dist (butlast left) right)))
                            (inc (get @cache key (lev-dist left (butlast right))))
                            (+ cost (get @cache key (lev-dist (butlast left) (butlast right)))))]
                    (do (swap! cache assoc key val) val)))))]
      (lev-dist left right))))

;;;; -----=====***** 102 (3 lines, 22 total) *****=====-----
(fn camel-case [word]
  (let [[lead & words] (clojure.string/split word #"-")]
    (apply str lead (map clojure.string/capitalize words))))

;;;; -----=====***** 103 (10 lines, 32 total) *****=====-----
(fn k-comb [n st]
  (let [gen-next 
         (fn [p-sets n-set]
           (into #{} (for [p p-sets n n-set
                           :when (not (contains? p n))]
                       (conj p n))))
        st-of-sts (map (fn [n] #{n}) st)]
    (loop [i 1 out-set st-of-sts]
      (if (= i n) (into #{} out-set)
        (recur (inc i) (gen-next out-set st))))))

;;;; -----=====***** 104 (16 lines, 48 total) *****=====-----
(fn write-roman [n]
  (let [split-digits 
          (fn [n] (map #(Integer/parseInt (str %)) (str n)))
        digits (into [] (reverse (split-digits n)))
        process-place
          (fn [n one-sym five-sym ten-sym]
            (cond
              (= n nil) ""
              (<= n 3) (apply str (take n (repeat one-sym)))
              (= n 4) (str one-sym five-sym)
              (< n 9) (apply str five-sym (take (- n 5) (repeat one-sym)))
              (= n 9) (str one-sym ten-sym)))]
    (str (process-place (get digits 3) "M" "M" "M")
         (process-place (get digits 2) "C" "D" "M")
         (process-place (get digits 1) "X" "L" "C")
         (process-place (get digits 0) "I" "V" "X"))))