;; Hmm, map. Okay.

;; So, the very first thought is "Use reduce!" but...reduce isn't lazy. So, we can't. So, lazy-seq ahoy! How might we do this thing?
;; Well. Make a lazy-seq that cons-es new members onto an output list.
(defn my-map 
  ([pred coll] (my-map pred []))
  ([pred out coll]
    (if (seq coll)
    (cons out (lazy-seq (my-map pred (pred out) (rest coll))))))

(my-map inc [2 3 4 5 6])

;; Mostly copied from http://clojure.org/lazy#Making%20Clojure%20Lazier--The%20victim%20-%20nil%20punning
;; I just searched for nil punning and lo and behold!
;; So, I don't really use cons, but DANG is it ideal for this situation!
(defn my-map [pred coll]
  (let [step (fn [p c]
                 (when-let [s (seq c)] ; when (seq c) is true, let s = (seq c), else return nil
                   (cons (pred (first s)) 
                         (my-map p (rest s)))))]
    (lazy-seq (step pred coll))))