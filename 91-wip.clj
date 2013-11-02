; Didn't I just write this?
; Delicious, delicious copypasta
(
(fn connected? [edges]
  (letfn [(fold [m n]
            (let [f-n (first n) l-n (last n)]
              (if (not= f-n l-n) (conj m f-n l-n) m)))
          (to-set [e]
            (apply conj #{} (flatten e)))]
  (if (= (to-set edges) (reduce fold #{} edges))
    true
    false)))
#{[1 2]})

; Oh, wait, it wants a list, not a set...
(
(fn connected? [edge-set]
  (letfn [(fold [m n]
            (let [f-n (first n) l-n (last n)]
              (if (not= f-n l-n) (conj m f-n l-n) m)))
          (to-set [e]
            (apply conj #{} (flatten e)))]
    (let [edges (apply conj [] edge-set)]
      (if (= (to-set edges) (reduce fold #{} edges))
        true
        false))))
#{[:a :a]})

; Ooooh, I see. A flaw in the algorithm. The special case of one edge.
; If it has one edge, it is *always* connected.
(
(fn connected? [edge-set]
  (if (= 1 (count edge-set)) true
    (letfn [(fold [m n]
              (let [f-n (first n) l-n (last n)]
                (if (not= f-n l-n) (conj m f-n l-n) m)))
            (to-set [e]
              (apply conj #{} (flatten e)))]
      (let [edges (apply conj [] edge-set)]
        (if (= (to-set edges) (reduce fold #{} edges))
          true
          false)))))
#{[:a :a]})

;   Okay, it turns out the algorithm I used previously is *fundamentally 
; flawed* Hah! Serves me right for blowing through it. It searches for 
; *unconnected* elements, rather than connectivity. It worked for the
; previous problem because it wasn't the main focus so it squeaked by the unit
; tests, but can't do that here.
;   This is where the "minimal learning" method of operation really bites you,
; because it would have been obvious if I were more studied in graph theory.

;   Right. Let's figure something out. Preferably that isn't a DFS/BFS.
;   We can group them into sets, basically. So, if we have [1 2] and [1 3] what
; that's really saying is that 1 is connected to 2 is connected to 3, so we 
; would end up with #{1 2 3}.
;   To keep track of this we can use a set of sets.
;   So, #{[1 2] [2 3] [3 1] [4 5] [5 6] [6 4]} would end up as 
; #{#{1 2 3} #{4 5 6}} - if there's only one set, they're connected.

; Searches any number of disjoint sets, and returns the one containing node
; nil if no such set
(defn get-containing [sets node]
  (reduce #(if (contains? %2 node) %2 %1) nil sets))

; Takes a set of sets and an edge, and combines the two appropriately
; If no set matches the edge, the edge becomes a free-floating set
(
(defn group [sets edge]
  (let [get-containing (fn [sets node]
          (reduce #(if (contains? %2 node) %2 %1) nil sets))
        contains-left (get-containing sets (first edge))
        contains-right (get-containing sets (last edge))]
    (cond
      (and (set? contains-left) (set? contains-right)) 
          (conj (disj sets contains-left contains-right) 
              (clojure.set/union contains-left contains-right))
      (set? contains-left) (conj (disj sets contains-left) 
          (apply conj contains-left edge))
      (set? contains-right) (conj (disj sets contains-right) 
          (apply conj contains-right edge))
      :else
      (conj sets (apply conj #{} edge)))))
#{#{1 2} #{3 4}} [1 4])

; Just count the results
(fn connected? [edges]
  (letfn [(group [sets edge]
                 (let [gt-cntns (fn [sets node]
                                  (reduce #(if (contains? %2 node) %2 %1) 
                                          nil 
                                          sets))
                       contains-left (gt-cntns sets (first edge))
                       contains-right (gt-cntns sets (last edge))]
                   (cond
                    (and (set? contains-left) (set? contains-right)) 
                        (conj (disj sets contains-left contains-right) 
                          (clojure.set/union contains-left contains-right))
                    (set? contains-left) (conj (disj sets contains-left) 
                                               (apply conj contains-left edge))
                    (set? contains-right) (conj (disj sets contains-right) 
                                                (apply conj contains-right edge))
                    :else
                    (conj sets (apply conj #{} edge)))))]
    (= 1 (count (reduce group #{} edges)))))