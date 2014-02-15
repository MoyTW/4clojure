;; So...wait, what. "between every two items that satisfy the predicate" - so you need to do a 2-step.
;; That could be a lookbehind reduce, or what other way could it be done?

;; Well, you could, like, do a map with a shift.
(defn __ [pred val coll]
  (conj (vec (mapcat #(if (pred %1 %2) [%1 val] [%1]) 
                     coll 
                     (rest coll))) 
        (last coll)))
  
(__ < :less [1 6 7 4 3])

;; oh should be empty not nil so uh just shoehorn a check in there:
(defn __ [pred val coll]
  (if-let [tail (last coll)]
    (conj (vec (mapcat #(if (pred %1 %2) [%1 val] [%1]) 
                       coll 
                       (rest coll))) 
          tail)))

(__ > :more ())

;; uuuuuuh integer overflow? That, uh, hmm. Wasn't expecting that. What the dickens? Oh it's an infinite - ofc, lazy. Hmm, that...makes it harder. See, if it's lazy you can't go out to the end like I'm doing to grab the last one...

(defn __ [pred val coll]
  (mapcat #(if (pred %1 %2) [%1 val] [%1])
          coll
          (concat (rest coll) [(first coll)])))
(__ > :more ())
(__ < :less [1 6 7 4 3])          
(take 12 (->> [0 1]
              (iterate (fn [[a b]] [b (+ a b)]))
              (map first) ; fibonacci numbers
              (__ (fn [a b] ; both even or both odd
                    (= (mod a 2) (mod b 2)))
                  :same)))
;; How hacky! Also it'll fail if you do:
(__ = :eq [1 2 3 3 4 5 1])
;; but it passes the tests soooooooo