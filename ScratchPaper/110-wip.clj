;; ...pronounciations? That, uh, that makes me think that this will be about something completely different.
;; Wow, that's a confusing algorithm.

;; Let's track it:
;; 11
;; 21
;; 12 11
;; 11 12 21
;; 31 22 11
;; 13 11 22 21
;; 11 13 21 32 11

;; So, basically, the form is "N of X". If you have 31 22 11 that becomes:
;;  1 of 3
;;  1 of 1
;;  2 of 2
;;  2 of 1
;; -> 13 11 22 21

;; How might we implement this? Well, we're consuming the sequence, grouping under specific conditions. Reduce comes to mind. Or, maybe we could partition it into the unique numbers, as follows:
;; (3 1 2 2 1 1) -> ((3) (1) (2 2) (1 1))
;; and then mapcat them to their counts.

;; Actually, I like that one.
(partition-by (fn [x] x) '(3 1 2 2 1 1))
(mapcat #(list (count %) (first %)) (partition-by (fn [x] x) '(3 1 2 2 1 1)))

(
(defn step [coll]
  (mapcat #(list (count %) (first %)) (partition-by (fn [x] x) coll)))
[2 1])

(defn weird [coll]
  (iterate step coll))

(take 3 (
(fn weird [coll]
  (letfn [(step [coll]
           (mapcat #(list (count %) (first %)) 
                   (partition-by (fn [x] x) coll)))]
    (iterate step coll)))
[1 1]))

;; Uuuh, oh. Right, first.
(fn weird [coll]
  (letfn [(step [coll]
           (mapcat #(list (count %) (first %)) 
                   (partition-by (fn [x] x) coll)))]
    (rest (iterate step coll))))