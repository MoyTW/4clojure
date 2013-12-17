;;;; -----=====***** 95 (9 lines, 9 total) *****=====----
(fn test-btree [[val l-chld r-chld :as node]]
  (cond
    (= node nil) true
    (or (not (or (= nil l-chld) (coll? l-chld)))
        (not (or (= nil r-chld) (coll? r-chld)))
        (= val nil) 
        (not= (count node) 3)) 
      false
    :else (and (test-btree l-chld) (test-btree r-chld))))

;;;; -----=====***** 96 (5 lines, 14 total) *****=====----
(fn check-sym [[val l-tree r-tree]]
  (letfn [(reverse-btree [[val l-chld r-chld :as node]]
           (if (= node nil) nil
             [val (reverse-btree r-chld) (reverse-btree l-chld)]))]
    (= l-tree (reverse-btree r-tree))))

;;;; -----=====***** 97 (8 lines, 22 total) *****=====----
(fn nth-row [n]
  (letfn [(next-row [row]
            (let [n-row (reduce (fn [[out l] n] 
                                  [(conj out (+ l n)) n]) 
                                [[] 0]
                                row)]
              (conj (first n-row) (last n-row))))]
    (nth (iterate next-row [1]) (dec n))))

;;;; -----=====***** 98 (7 lines, 29 total) *****=====----
(fn eq-classes [pred domain]
  (loop [classes #{} d domain]
    (let [class (filter #(= (pred %) (pred (first d))) d)
          next-classes (conj classes (into #{} class))
          rest-domain (apply disj d class)]
      (if (empty? rest-domain) next-classes
        (recur next-classes rest-domain)))))

;;;; -----=====***** 99 (2 lines, 31 total) *****=====----
(fn product-digits [x y]
  (map #(Integer/parseInt (str %)) (str (* x y))))

;;;; -----=====***** 100 (9 lines, 40 total) *****=====----
(fn lcm-mult [& args]
  (letfn [(gcd [a b]
            (loop [a a b b]
              (let [[lrg sma] (reverse (sort [a b]))]
                (if (= lrg sma) lrg
                  (recur (- lrg sma) sma)))))
          (lcm [a b]
             (/ (* a b) (gcd a b)))]
    (reduce lcm args)))