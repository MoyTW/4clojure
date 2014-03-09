;; 4Clojure Problem 137. Digits and bases
;; url: http://www.4clojure.com/problem/137
(fn __ [n base]
  (let [v (mod n base)
        r (int (/ n base))]
    (if (zero? r)
        [v]
        (conj (__ r base) v))))