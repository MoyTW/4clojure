;; So, basically, string->map
;; First letter is suit, second letter is value, sooooo

(def suits {\D :diamond \H :heart \C :club \S :spade})
(def values {\2 0 \3 1 \4 2 \5 3 \6 4 \7 5 \8 6 \9 7 \T 8 \J 9 \Q 10 \K 11 \A 12})

(def a (into [] "H5"))

{:suit (get suits (first a)) :rank (get values (second a))}

(fn resolve-card [[suit rank]]
  (let [suits {\D :diamond \H :heart \C :club \S :spade}
        ranks {\2 0 \3 1 \4 2 \5 3 \6 4 \7 5 \8 6 \9 7 \T 8 \J 9 \Q 10 \K 11 \A 12}]
    {:suit (get suits suit) :rank (get ranks rank)}))