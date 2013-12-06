;; Hmm. Okay.
;; First thing to note is that, well, the sequences given are strictly increasing. So. I guess we can count on that.
;; The temptation to slam it with the flatten hammer is overwhelming but foolish - it would lose too much nesting data.
;; Further note that they never go back up. That is, you won't have to climb the data structure back up.
;; I am unsure if we can avoid loop/recur here.

(
(fn what [n coll]
  (loop [cnt 0 out [] in coll]
    (let [head (first in)]
    (if (coll? head)
      (recur cnt (conj out []) head)
      (if (> (+ cnt head) n)
        out
        (recur (+ cnt head) (conj out head) (rest in)))))))
10 [1 2 [3 [4 5] 6] 7])

;; ...that does totally the wrong thing.

;; Okay, before I spit out another "Let's toss these pieces of code together in a vaguely problem-solve-y manner!" let's think about what we want to be doing here.
;; So, we're doing nesting. Basically, "If the addition is a list, nest one level further."
;; ...the thing is, I don't know how to actually handle deep nesting! See, the way I usually work on immutable data structures is by breaking them sideways, not be breaking them down-up. To do that you'd need to...oh. Wait.
;; Recur downwards.
;; ...remember that we never come back up. So, we can create two functions.

;; First, we've got our loop/recur, traverses horizontally.
;; Second, we've got our recursive function, which traverses vertically.
;; The loop/recur will call this function when it finds a collection, and then add that collection as the last member of the current level.

(
(fn what [n coll]
  (letfn [(process-next [cnt in]
            (loop [cnt cnt out [] in in]
              (let [head (first in)]
                (cond
                  (= head nil) out
                  (coll? head) (conj out (process-next cnt head))
                  (> (+ cnt head) n) out
                  :else (recur (+ cnt head) (conj out head) (rest in))))))]
    (process-next 0 coll)))
10 [1 2 [3 [4 5] 6] 7])
;10 [1 2 [3]])
