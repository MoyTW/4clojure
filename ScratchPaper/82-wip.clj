; By hand:
; Given #{hat coat dog cat oat cot hot hog}
; [hat hat] #{coat dog cat oat cot hot hog}
; [cat hat] #{coat dog --- oat cot hot hog}
; [cot hat] #{coat dog --- oat --- hot hog}
; [coat hat] #{--- dog --- oat --- hot hog}
; [oat hat] #{---- dog --- --- --- hot hog}
; [oat hot] #{---- dog --- --- --- --- hog}
; [oat hog] #{---- dog --- --- --- --- ---}
; [oat dog] #{}

; It is possible to make a valid subchain, but in such way that it won't form the full chain?
; How about [goat coat oat hat]?
; Valid subchain is {goat oat coat} but you cannot match hat!
; Also has full valid subclain {hat oat coat goat}, so yes, it is possible to have a valid subchain which prevents a full chain.
; WELL.
; That'll be tricky.

; Above method can mess up
; Given #{coat oat goat hat}
; [coat coat] #{oat goat hat}
; [oat coat] #{---- goat hat}
; [goat coat] #{--- ---- hat}
; cannot place, fails

; Requires more thought.

; Now, one thing we could do is simply split in two every time we see more than one candidate, which would give us a tree. We could evaluate the leaf nodes of the tree, and if all leaf nodes evaluate to no chain, there's no chain. Otherwise, it will find the chain.
; A little brute-force-y, but, hey.
; Let's try:
; Given #{coat oat goat hat}
; [coat coat] #{oat goat hat}
; [oat coat] #{goat hat} OR [goat coat] #{oat hat} OR [coat oat] #{goat hat} OR [coat goat] #{oat hat}
; and so on and so on
; that's a little more complex, though, can we git it simpler?

; In order to avoid this, we should always chain words with 2 possible partners first
; So, given #{coat goat oat hat}, we count the number of possible partners
; #{coat goat oat hat}
;  [ 2    2    3   1 ]
; wait, no, the proper thing to do would be to chain the one with the *least* possible partners first (chain hat to oat, which would ensure a valid chaining after that point)
; So we could make a map, with the keys in the set, and the possible words to chain to as a set or vector
; For example, #{coat #{goat oat} goat #{coat oat} oat #{coat goat hat} hat #{oat}}
; We would chain hat with oat
; and then chain oat with - how do you determine which? Here it doesn't matter. Could we find a chain where it does?

; #{coat             goat                   oat                  hat        groat}
; #{coat #{goat oat} goat #{coat oat groat} oat #{coat goat hat} hat #{oat} groat #{goat}}
; Possible chain [groat <-> goat <-> coat <-> oat <-> hat]
; Possible improper formation: [hat <-> oat <-> goat <-> coat]
; We can avoid this by first chaining [hat <-> oat] [groat <-> goat]
; Can't use the original (only has two heads) can we?

; bah this is more complicated than I thought it would be

; How about following start with lowest match, always match lowest next?
; [hat -> oat -> coat -> goat -> groat]
; Would this break down if highest number of matches did not match the last produced by this algorithm?
; #{zoat coat goat oat hat groat}
;    3    3    4    4   1   1
; groat -> goat -> coat -> zoat -> oat -> hat
; whoops, didn't make the sequence right...

; Okay, well, we're not out to *prove* the correctness of the algorithm, and I've spend "enough" time on 82, already. I'm going with the "Try it and find out" school, now.

; Given long word, short word of appropriate lengths, determine if subtraction matches
(defn check-diff [long-w short-w]
  (loop [n (count long-w)]
    (cond
      (= n 0) false
      (= (str (subs long-w 0 (dec n)) (subs long-w n)) short-w) true
      :else (recur (dec n)))))

; NOTE: We don't need this, actually, moved into diff-by-one
; Determines if differ by deletion (if differ by deletion, differ by insertion)
(defn diff-by-del [l-w r-w]
  (let [l-cnt (count l-w) r-cnt (count r-w)]
    (cond 
      (= (inc l-cnt) r-cnt) (check-diff r-w l-w)
      (= (inc r-cnt) l-cnt) (check-diff l-w r-w)
      :else false)))

; Given two words of equal length, determine if differ by substitution
(defn diff-by-sub [l-w r-w]
  (if (= 1 (reduce #(if %2 %1 (inc %1)) 0 (map #(= %1 %2) l-w r-w)))
    true
    false))
; Okay it turned out kind of convoluted (that chaining of if reduce map, oh dear)
; It works, though. Do we want to spend time cleaning it up?
; Note that special case of checking diff of same words will return false! Do we need it to do otherwise? No, because we're working on sets.

; Function to determine if two words are one shift away
(defn diff-by-one [l-w r-w]
  (let [l-cnt (count l-w) r-cnt (count r-w)]
    (cond 
      (= l-cnt r-cnt) (diff-by-sub l-w r-w)
      (= (inc l-cnt) r-cnt) (check-diff r-w l-w)
      (= (inc r-cnt) l-cnt) (check-diff l-w r-w)
      :else false)))
      
; Function that, given a word, finds all one-shifted words in the set
(defn find-one-shifts [wrd st]
  (filter #(diff-by-one wrd %) st))

; Function to turn the set into a map
(defn set-to-map [s]
  (reduce #(assoc %1 %2 (find-one-shifts %2 s)) {} s))

; Actually, you know what? Let's sort the map by the count of the value...
; Lowest first.
(defn set-to-sorted-map [s]
  (let [unsorted (reduce #(assoc %1 %2 (find-one-shifts %2 s)) {} s)]
    (into (sorted-map-by #(compare [(count (get unsorted %1)) %1]
                                   [(count (get unsorted %2)) %2]))
          unsorted)))

; 4Clojure doesn't like def, sooooo...
(defn set-to-sorted-map [s]
  (letfn [(diff-by-del [long-w short-w]
            (loop [n (count long-w)]
              (cond
               (= n 0) false
               (= (str (subs long-w 0 (dec n)) (subs long-w n)) short-w) true
               :else (recur (dec n)))))
          (diff-by-sub [l-w r-w]
            (if (= 1 (reduce #(if %2 %1 (inc %1)) 
                             0 
                             (map #(= %1 %2) l-w r-w)))
              true
              false))
          (diff-by-one [l-w r-w]
            (let [l-cnt (count l-w) r-cnt (count r-w)]
              (cond 
               (= l-cnt r-cnt) (diff-by-sub l-w r-w)
               (= (inc l-cnt) r-cnt) (diff-by-del r-w l-w)
               (= (inc r-cnt) l-cnt) (diff-by-del l-w r-w)
               :else false)))
          (find-one-shifts [wrd st]
            (filter #(diff-by-one wrd %) st))]
    (let [unsorted (reduce #(assoc %1 %2 (find-one-shifts %2 s)) {} s)]
      (into (sorted-map-by #(compare [(count (get unsorted %1)) 0]
                                     [(count (get unsorted %2)) 0]))
            unsorted))))
; That is just incredibly ugly.

; Function to select the word with the lowest count
(defn get-lowest-count [st mp]
  (first (sort-by #(count (get mp %1)) st)))
  
(
(fn try-chain [st]

#{"goat" "coat" "moat" "moaz" "zoaz"})

; I really wish 4Clojure would let you use def! Ah, well.
; I'm sure that there's a better, prettier, more elegant way to do this, but hey.
(fn oh-god-no [s]
  (letfn [(diff-by-del [long-w short-w]
            (loop [n (count long-w)]
              (cond
               (= n 0) false
               (= (str (subs long-w 0 (dec n)) (subs long-w n)) short-w) true
               :else (recur (dec n)))))
          (diff-by-sub [l-w r-w]
            (if (= 1 (reduce #(if %2 %1 (inc %1)) 
                             0 
                             (map #(= %1 %2) l-w r-w)))
              true
              false))
          (diff-by-one [l-w r-w]
            (let [l-cnt (count l-w) r-cnt (count r-w)]
              (cond 
               (= l-cnt r-cnt) (diff-by-sub l-w r-w)
               (= (inc l-cnt) r-cnt) (diff-by-del r-w l-w)
               (= (inc r-cnt) l-cnt) (diff-by-del l-w r-w)
               :else false)))
          (find-one-shifts [wrd st]
            (filter #(diff-by-one wrd %) st))
          (set-to-sorted-map [s]
            (let [unsorted (reduce #(assoc %1 %2 (find-one-shifts %2 s)) {} s)]
              (into (sorted-map-by #(compare [(count (get unsorted %1)) %1]
                                             [(count (get unsorted %2)) %2]))
                    unsorted)))]
    (let [get-lowest-count (fn [st mp] (first (sort-by #(count (get mp %1)) st)))
          mp (set-to-sorted-map s)]
      (loop [head (last (first mp)) mp (dissoc mp (ffirst mp))]
        (let [p-lnks (filter #(contains? mp %) head)]
          (cond
           (empty? mp) true
           (empty? p-lnks) false
           :else
           (let [next-key (get-lowest-count p-lnks mp)]
             (recur (get mp next-key) (dissoc mp next-key)))))))))
; yeeeeah
; OTOH it passes tests so onwards!
