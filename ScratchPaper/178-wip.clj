;; sooo we can do these in sequence I guess
;; I'll go get the solution from 128
(defn resolve-card [[suit rank]]
  (let [suits {\D :diamond \H :heart \C :club \S :spade}
        ranks (apply hash-map (interleave "23456789TJQKA" (range)))]
    {:suit (suits suit) :rank (ranks rank)}))

(defn __ [coll]
  (let [hand (map resolve-card coll)]
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

(__ ["HA" "D2" "H3" "C9" "DJ"])

;; Handling Ace is low or high or just high?
(defn straight-flush [hand]
  (let [distinct-suits (distinct (map :suit hand))
        sorted-ranks (sort (map :rank hand))]
    (and (= 1 (count distinct-suits))
         (= 4 (- (last sorted-ranks)
                 (first sorted-ranks))))))
(__ ["HA" "HK" "HQ" "HJ" "HT"])

(defn four-of-a-kind [hand]
  (let [ranks (map :rank hand)
        [x y] (sort (map count ((juxt filter remove) #{(first ranks)} ranks)))]
    (and (= 2 (count (distinct ranks)))
         (= 1 x)
         (= 4 y))))
(= :four-of-a-kind (__ ["HA" "DA" "CA" "SA" "DJ"]))

(defn full-house [hand]
  (let [ranks (map :rank hand)
        [x y] (sort (map count ((juxt filter remove) #{(first ranks)} ranks)))]
    (and (= 2 (count (distinct ranks)))
         (= 2 x)
         (= 3 y))))
(= :full-house (__ ["HA" "DA" "CA" "HJ" "DJ"]))

(defn flush-hand [hand]
  (-> :suit (map hand) (distinct) (count) (= 1)))
(= :flush (__ ["HA" "HK" "H2" "H4" "HT"]))

(defn straight [hand]
  (let [sorted-ranks (sort (map :rank hand))]
    (or (and (= 5 (count (distinct sorted-ranks)))
             (= 4 (- (last sorted-ranks) (first sorted-ranks))))
        (= sorted-ranks [0 1 2 3 12]))))
(= :straight (__ ["HA" "H2" "S3" "D4" "C5"]))
(= :straight (__ ["HA" "DK" "HQ" "HJ" "HT"]))

(defn three-of-a-kind [hand]
  (some #{3} (vals (frequencies (map :rank hand)))))
(= :three-of-a-kind (__ ["HA" "DA" "CA" "HJ" "HT"]))

(defn two-pair [hand]
  (= [1 2 2] (sort (vals (frequencies (map :rank hand))))))
(= :two-pair (__ ["HA" "DA" "HQ" "SQ" "HT"]))

(defn pair [hand]
  (= [1 1 1 2] (sort (vals (frequencies (map :rank hand))))))
