;; There are a lot of words whose meanings I don't know in that problem statement.
;; Oh. Oh! It's boolean logic. I was never amazing at boolean logic. I do have vague memories of the subject, though.
;; I think I had to learn about K-maps, and that class I got a B in? I didn't do so hot. Plus it seems all the memory has fallen out of my head!

;; So basically we need to get from here (using example 1):
;; (!a  B  C !d) +
;; ( A !b !c !d) +
;; ( A !b !c  D) +
;; ...
;; ( A  B  C !d)
;; to
;; (A + !c) + (A + !b) + (B C !d)

;; How do we do this thing?
;; Well, there are a bunch of rules you could apply to simplify it, I think.

;; hmm
;; http://sce.umkc.edu/~hieberm/281_new/lectures/simplification/simplification.html
;; http://en.wikipedia.org/wiki/Quine%E2%80%93McCluskey_algorithm
;; I don't want to mess around with trying to apply rules programatically
;; I haven't the slightest idea how to do a K-map in code. Hmm. Making it wouldn't be excessively hard, but how would I get it to process properly? As in, how would I get it to recognize where it should put its 'circles'?
;; Okay let's look at McCluskey.

(def p6-in 
  {[0] "0000"
   [1] "0100"
   [2] "1100"
   [3] "0001"
   [4] "0101"
   [5] "1101"})
"0-0-"
"-10-"

; 0 0
; 1 1
;   3
; 2 2
;   4
; 3 5
; bah there's no way I can do this in nodepad. TO THE SPREADSHEET

;; Okay. I think I know how the algorithm goes.

(defn num-ones [coll]
  (count (filter #{\1} coll)))
(num-ones "0101")
(defn diff-by-one [left right]
  (= 1 (count (filter false? (map = left right)))))
(diff-by-one "-011" "-010")
(diff-by-one "-011" "-011")
(diff-by-one "-011" "-000")
(defn produce-diff [left right]
  (map #(if (not= %1 %2) \- %1) left right))
(produce-diff "-011" "-010")

(def state-zero (group-by #(num-ones (second %)) p6-in))

(defn merge-terms [n state]
  (let [less (state n)
        more (state (inc n))]
    (for [[l-terms l-func] less
          [m-terms m-func] more
          :when (diff-by-one l-func m-func)]
       [(concat l-terms m-terms) (produce-diff l-func m-func)])))
(merge-terms 1 state-zero)

(defn step [state]
  (let [r (range (dec (count state)))]
    (group-by #(num-ones (second %)) (mapcat #(merge-terms % state) r))))
(step state-zero)

(step (step state-zero))
(step (step (step state-zero)))

;; Okay, so, we can generate the miniterms easily enough, how do we tell if they're final? Well, they're final if they don't get merged into the next round, soooo...

(def state-one (step state-zero))

(def zero-minis (map (comp set first) (mapcat second state-zero)))
(def one-minis (map (comp set first) (mapcat second state-one)))

(clojure.set/subset? #{1} #{1 2})
(map #(clojure.set/subset? #{1} %) one-minis)
(for [l-term zero-minis]
  (some true? (map #(clojure.set/subset? l-term %) one-minis)))
  
(defn find-unmerged [old-state new-state]
  (let [old-minis (map (comp set first) (mapcat second old-state))
        new-minis (map (comp set first) (mapcat second new-state))
        new-map (apply hash-map (apply concat (map #(vector (into #{} (first %)) (second %)) (mapcat second old-state)))) ; yeah this is pretttty dumb
        unmerged (for [l-term old-minis
                       :when (every? false? (map #(clojure.set/subset? l-term %) new-minis))]
                    (new-map l-term))]
    (into #{} unmerged)))
(find-unmerged (step (step state-zero)) (step (step (step state-zero))))
;; wait no
(apply hash-map (apply concat (mapcat second new-state)))

;; okay now
(defn step [state final]
  (let [r (butlast (sort (keys state)))
        next-state (group-by #(num-ones (second %)) 
                             (mapcat #(merge-terms % state) r))
        unmerged (find-unmerged state next-state)]
    (if (seq next-state)
        (recur next-state (concat final unmerged))
        (concat final unmerged))))
(step state-zero #{})
(defn __ [func-map]
  (step (group-by #(num-ones (second %)) func-map) #{}))
  
;; Test:
#{#{'a 'B 'C 'd}
 #{'A 'b 'c 'd}
 #{'A 'b 'c 'D}
 #{'A 'b 'C 'd}
 #{'A 'b 'C 'D}
 #{'A 'B 'c 'd}
 #{'A 'B 'c 'D}
 #{'A 'B 'C 'd}}
(def p1-fmap
  {[0] "0110"
   [1] "1000"
   [2] "1001"
   [3] "1010"
   [4] "1011"
   [5] "1100"
   [6] "1101"
   [7] "1110"})
(__ p6-in) ; ((\- \1 \0 \-) (\0 \- \0 \-))
(__ p1-fmap) ; ((\- \1 \1 \0) (\1 \- \- \0) (\1 \0 \- \-) (\1 \- \0 \-))
;; Well, that's not right at all.