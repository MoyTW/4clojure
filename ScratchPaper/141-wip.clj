;; soooo, function returning a function, then. That detects tricks, given a - ah, ok. So, closuretimes looks like.

(defn given-trump [trick trump]
  (if trump
      (->> trick
           (filter #(= (% :suit) trump))
           (sort-by :rank)
           (last))))
(given-trump [{:suit :spade :rank 2} {:suit :club :rank 10}] :club)
(given-trump [{:suit :spade :rank 2} {:suit :club :rank 10}] :spade)
(given-trump [{:suit :spade :rank 5} {:suit :spade :rank 9} {:suit :spade :rank 2} {:suit :club :rank 10}] :spade)

(defn given-trump [trick trump]
  (if trump
      (->> trick
           (filter #(= (% :suit) trump))
           (sort-by :rank)
           (last))
      (let [lead ((first trick) :suit)]
        (do (prn lead)
        (->> trick
             (filter #(= (% :suit) lead))
             (sort-by :rank)
             (last))))) )
(= {:suit :club :rank 9}  
   (given-trump [{:suit :club :rank 4} {:suit :club :rank 9}] nil))
(= {:suit :spade :rank 2}
   (given-trump [{:suit :spade :rank 2} {:suit :club :rank 10}] nil))

(defn given-trump [trick trump]
  (let [winning-suit (if trump trump ((first trick) :suit))]
    (->> trick
         (filter #(= (% :suit) winning-suit))
         (sort-by :rank)
         (last))))

(fn __ [trump]
  (fn [trick]
    (let [winning-suit (if trump trump ((first trick) :suit))]
      (->> trick
           (filter #(= (% :suit) winning-suit))
           (sort-by :rank)
           (last)))))