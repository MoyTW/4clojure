; ...so, let's see.
; ...hmm. Okay...how about...filter?
; That'd mean "Recurse until empty" - a loop/recur. OTOH, filtering seems like a really good way to solve it.
; Heck, let's go for it.

(defn eq-classes [pred domain]
  (loop [classes #{} d domain]
    (let [class (filter #(= (pred %) (pred (first d))) d)
          next-classes (conj classes (into #{} class))
          rest-domain (apply disj d class)]
      (if (empty? rest-domain) next-classes
        (recur next-classes rest-domain)))))
      