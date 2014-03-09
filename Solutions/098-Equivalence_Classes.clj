;; 4Clojure Problem 98. Equivalence Classes
;; url: http://www.4clojure.com/problem/98
(fn eq-classes [pred domain]
  (loop [classes #{} d domain]
    (let [class (filter #(= (pred %) (pred (first d))) d)
          next-classes (conj classes (into #{} class))
          rest-domain (apply disj d class)]
      (if (empty? rest-domain) next-classes
        (recur next-classes rest-domain)))))