;; 4Clojure Problem 178. Best Hand
;; url: http://www.4clojure.com/problem/178
(fn __ [coll]
  (let [resolve-card (fn resolve-card [[suit rank]]
                       (let [suits {\D :diamond \H :heart \C :club \S :spade}
                             ranks (apply hash-map (interleave "23456789TJQKA" (range)))]
                         {:suit (suits suit) :rank (ranks rank)}))
        straight-flush
          (fn straight-flush [hand]
            (let [distinct-suits (distinct (map :suit hand))
                  sorted-ranks (sort (map :rank hand))]
              (and (= 1 (count distinct-suits))
                   (= 4 (- (last sorted-ranks)
                           (first sorted-ranks))))))
        four-of-a-kind
          (fn four-of-a-kind [hand]
            (let [ranks (map :rank hand)
                  [x y] (sort (map count ((juxt filter remove) #{(first ranks)} ranks)))]
              (and (= 2 (count (distinct ranks)))
                   (= 1 x)
                   (= 4 y))))
        full-house
          (fn full-house [hand]
            (let [ranks (map :rank hand)
                  [x y] (sort (map count ((juxt filter remove) #{(first ranks)} ranks)))]
              (and (= 2 (count (distinct ranks)))
                   (= 2 x)
                   (= 3 y))))
        flush-hand
          (fn flush-hand [hand]
            (-> :suit (map hand) (distinct) (count) (= 1)))
        straight
          (fn straight [hand]
            (let [sorted-ranks (sort (map :rank hand))]
              (or (and (= 5 (count (distinct sorted-ranks)))
                       (= 4 (- (last sorted-ranks) (first sorted-ranks))))
                  (= sorted-ranks [0 1 2 3 12]))))
        three-of-a-kind
          (fn three-of-a-kind [hand]
            (some #{3} (vals (frequencies (map :rank hand)))))
        two-pair
          (fn two-pair [hand]
            (= [1 2 2] (sort (vals (frequencies (map :rank hand))))))
        pair
          (fn pair [hand]
            (= [1 1 1 2] (sort (vals (frequencies (map :rank hand))))))
        hand (map resolve-card coll)]
    (cond
      (straight-flush hand) :straight-flush
      (four-of-a-kind hand) :four-of-a-kind
      (full-house hand) :full-house
      (flush-hand hand) :flush
      (straight hand) :straight
      (three-of-a-kind hand) :three-of-a-kind
      (two-pair hand) :two-pair
      (pair hand) :pair
      :else :high-card)))