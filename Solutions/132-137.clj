;;;; -----=====***** 132 (4 lines, 4 total) *****=====-----
; Hey what happens if you run (__ = :eq [1 2 2 1]) do you think?
(fn __ [pred val coll]
  (mapcat #(if (pred %1 %2) [%1 val] [%1])
          coll
          (concat (rest coll) [(first coll)])))

;;;; -----=====***** 134 (2 lines, 6 total) *****=====-----
(fn __ [k m]
  (= nil (get m k :not-present)))

;;;; -----=====***** 135 (3 lines, 9 total) *****=====-----
(fn __ [initial & args]
  (let [ops (partition 2 args)]
    (reduce #((first %2) %1 (second %2)) initial ops)))

;;;; -----=====***** 137 (6 lines, 15 total) *****=====-----    
(fn __ [n base]
  (let [v (mod n base)
        r (int (/ n base))]
    (if (zero? r)
        [v]
        (conj (__ r base) v))))

;;; 138 is another long one, so I won't lump it in with these.
;;; The irregular numbering and increasing frequency of gargantuan solutions
;;;  is starting to mess up my system!