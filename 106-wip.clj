; Hey, another math-y one. Let's seeeeee.

; (= 9 (__ 9 2))  ; 9 18 20 10 12 6 8 4 2
; (= 5 (__ 9 12)) ; 9 11 22 24 12

; How do we find the shortest path?
; Okay. We've got a space of values here. Or, well, I guess it's a graph.
; Each even node has 3 outgoing connections - double, halve, +2
; Odd nodes have 2 outgoing connections - double, +2
; I suppose we could just sort of...BFS our way to the end (start at 9, end at 12). Inelegant? Yeah, well. Brute force, and all.

; Example:
; [9]
; [18 11]
; [36 9 11] [22 13]
; this blows up pretty fast (ofc it does, it between triples and doubles each iteration)

; My goto shortest path is Djikstra's because his name is very unique. Enlightened of me, I know. However! The costs are always one here, so we don't actually need to use it.
; In fact we can just use an exploding set, like so:
(defn gen-paths [n]
  (if (odd? n) [(* n 2) (+ n 2)]
    [(* n 2) (/ n 2) (+ n 2)]))
    
(defn next-set [st]
  (into #{} (mapcat gen-paths st)))
  
(defn search [nd-fst nd-lst]
  (loop [i 1 node-set #{nd-fst}]
    (if (contains? node-set nd-lst) i
      (recur (inc i) (next-set node-set)))))

(= 9 (search 9 2))
(= 5 (search 9 12))

(fn search [nd-fst nd-lst]
  (letfn [(gen-next [st]
             (into #{} 
               (mapcat (fn [n]
                         (if (odd? n) [(* n 2) (+ n 2)]
                         [(* n 2) (/ n 2) (+ n 2)]))
                       st)))]
  (loop [i 1 node-set #{nd-fst}]
    (if (contains? node-set nd-lst) i
      (recur (inc i) (gen-next node-set))))))