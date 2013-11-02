;   Hmm, theory. Graph theory! Well.
;   I...think I remember hearing something about this problem from my math
; classes. Something, something, each edge once and only once, bridges of
; Prague or something?
;   Wait I just looked it up and it was Konigsberg. Seven bridges of 
; Konigsberg.
;   So apparently, it boils down to: does each node have an even number of 
; edges? If so, you can make a tour. If not, you cannot.
;   ...so all we need to do is count the number of times each element shows up,
; and if it's even, we've a path, if otherwise, we've not. Also the graph must
; be connected.
;   So that breaks it up into two chunks, I guess. First, test for connectivity
; and if so, test for a path.

; record a node into a map
(defn record-node [m n]
  (let [f-n (first n) 
        l-n (last n)
        with-f (assoc m f-n (inc (get m f-n 0)))]
    (assoc with-f l-n (inc (get with-f l-n 0)))))

; process edges into map, check if any are odd
(defn assumes-connected-euler? [edge-vec]
  (letfn [(record-node [m n]
            (let [f-n (first n) 
                  l-n (last n)
                  with-f (assoc m f-n (inc (get m f-n 0)))]
              (assoc with-f l-n (inc (get with-f l-n 0)))))]
  (if (empty? (->> edge-vec
                   (reduce record-node {})
                   (filter #(odd? (last %)))))
      true
      false)))
; decided to try using ->> instead of dense nesting
; I like the way it looks here!

; Wait, hold on, this is checking for an Euler *circuit*, not an Euler *trail*
; A *trail* can have up to two odd-degree nodes.
; So...well, gives me a chance to clean up the formatting, too.
(defn assumes-connected-euler? [edge-vec]
  (letfn [(record-node [m n]
            (let [f-n (first n) 
                  l-n (last n)
                  with-f (assoc m f-n (inc (get m f-n 0)))]
              (assoc with-f l-n (inc (get with-f l-n 0)))))]
  (->> edge-vec
       (reduce record-node {})
       (filter #(odd? (last %)))
       (count)
       (>= 2))))
      
;   So there's the "If it's connected" part, now we've got to write the "Is it
; connected?" part. A simple touches-all-connected search (DFS, BFS) compared
; to all the nodes in the graph will tell you if it's connected.
;   Wait, hold on. We have edges, not nodes. Actually...we can some something
; completely different. Create a set of nodes from the edges which span two
; nodes, if that set is equal to the set of all nodes, it's connected.
;   Could do!
(defn connected? [edges]
  (letfn [(fold [m n]
            (let [f-n (first n) l-n (last n)]
              (if (not= f-n l-n) (conj m f-n l-n) m)))
          (to-set [e]
            (apply conj #{} (flatten e)))]
  (if (= (to-set edges) (reduce fold #{} edges))
    true
    false)))

;   There's probably a way to refactor the two to, you know, properly combine,
; instead of doing my frankenstien-monster-thing.
(fn assumes-connected-euler? [edge-vec]
  (letfn [(record-node [m n]
                       (let [f-n (first n) 
                             l-n (last n)
                             with-f (assoc m f-n (inc (get m f-n 0)))]
                         (assoc with-f l-n (inc (get with-f l-n 0)))))
          (fold [m n]
                (let [f-n (first n) l-n (last n)]
                  (if (not= f-n l-n) (conj m f-n l-n) m)))
          (to-set [edges]
                  (apply conj #{} (flatten edges)))
          (connected? [edges]
                      (if (= (to-set edges) (reduce fold #{} edges))
                        true
                        false))]
    (if-not (connected? edge-vec) false
      (->> edge-vec
           (reduce record-node {})
           (filter #(odd? (last %)))
           (count)
           (>= 2)))))