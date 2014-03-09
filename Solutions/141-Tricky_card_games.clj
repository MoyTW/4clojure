;; 4Clojure Problem 141. Tricky card games
;; url: http://www.4clojure.com/problem/141
(fn __ [trump]
  (fn [trick]
    (let [winning-suit (if trump trump ((first trick) :suit))]
      (->> trick
           (filter #(= (% :suit) winning-suit))
           (sort-by :rank)
           (last)))))