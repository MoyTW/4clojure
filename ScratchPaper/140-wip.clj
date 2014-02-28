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

;; Okay, now must filter:
(defn step [state final]
  (let [r (butlast (sort (keys state)))
        next-state (group-by #(num-ones (second %)) 
                             (mapcat #(merge-terms % state) r))
        unmerged (find-unmerged state next-state)]
    (if (seq next-state)
        (recur next-state (concat final unmerged))
        [state (concat final unmerged)])))

(defn __ [func-map]
  (step (group-by #(num-ones (second %)) func-map) #{}))
(__ p1-fmap)
; [{1 [[(1 2 3 4) (\1 \0 \- \-)]
;      [(1 2 5 6) (\1 \- \0 \-)] 
;      [(1 3 2 4) (\1 \0 \- \-)]
;      [(1 3 5 7) (\1 \- \- \0)]
;      [(1 5 2 6) (\1 \- \0 \-)]
;      [(1 5 3 7) (\1 \- \- \0)]]}
;  ((\- \1 \1 \0) (\1 \- \- \0) (\1 \0 \- \-) (\1 \- \0 \-))]
;; No, actually, barking up the wrong tree there. We just want the unmerged set, but with the miniterms. So:
(defn step [state final]
  (let [r (butlast (sort (keys state)))
        next-state (group-by #(num-ones (second %)) 
                             (mapcat #(merge-terms % state) r))
        unmerged (find-unmerged state next-state)]
    (if (seq next-state)
        (recur next-state (concat final unmerged))
        (concat final unmerged))))
(defn find-unmerged [old-state new-state]
  (let [old-minis (map (comp set first) (mapcat second old-state))
        new-minis (map (comp set first) (mapcat second new-state))
        new-map (apply hash-map (apply concat (map #(vector (into #{} (first %)) (second %)) (mapcat second old-state)))) ; yeah this is pretttty dumb
        unmerged (for [l-term old-minis
                       :when (every? false? (map #(clojure.set/subset? l-term %) new-minis))]
                    [l-term (new-map l-term)])]
    (into #{} unmerged)))
(__ p1-fmap)
; ([#{0 7} (\- \1 \1 \0)]
;  [#{1 2 5 6} (\1 \- \0 \-)]
;  [#{1 3 5 7} (\1 \- \- \0)]
;  [#{1 2 3 4} (\1 \0 \- \-)])
;; Okay.

(filter #(contains? (first %1) 0) (__ p1-fmap))

(defn get-essential [mini unmerged]
  (let [covers (filter #(contains? (first %1) mini) unmerged)]
    (if (= 1 (count covers))
        [(second (first covers))]
        nil)))

(defn find-essentials [miniterms unmerged]
  (mapcat #(get-essential % unmerged) miniterms))
(find-essentials (apply concat (keys p1-fmap)) (__ p1-fmap))

;; aaand, okay, put it into __

(defn __ [func-map]
  (let [miniterms (apply concat (keys p1-fmap))
        implicants (step (group-by #(num-ones (second %)) func-map) #{})]
  (find-essentials miniterms implicants)))
(__ p1-fmap)
;; Hopefully all of the test cases are completely covered so we don't need to do the final trial pass.

;; Okay how to get from the given format to a better one? Mine is the better one, of course. As if there was any question about it.
(def conversion
  {'A [0 \1]
   'a [0 \0]
   'B [1 \1]
   'b [1 \0]
   'C [2 \1]
   'c [2 \0]
   'D [3 \1]
   'd [3 \0]})
(map conversion #{'a 'B 'C 'd})
(apply str (map second (sort-by first (map conversion #{'a 'B 'C 'd}))))

(defn func-to-string [coll]
  (apply str (map second (sort-by first (map conversion coll)))))

(def p1-strs 
  (map func-to-string
       #{#{'a 'B 'C 'd}
         #{'A 'b 'c 'd}
         #{'A 'b 'c 'D}
         #{'A 'b 'C 'd}
         #{'A 'b 'C 'D}
         #{'A 'B 'c 'd}
         #{'A 'B 'c 'D}
         #{'A 'B 'C 'd}}))
(= (sort p1-strs) (sort (vals p1-fmap)))
(apply merge (map-indexed #(hash-map [%1] %2) (sort p1-strs)))
(defn to-funcmap [coll]
  (apply merge (map-indexed #(hash-map [%1] %2) 
               (sort (map func-to-string coll)))))
(to-funcmap 
  #{#{'a 'B 'C 'd}
    #{'A 'b 'c 'd}
    #{'A 'b 'c 'D}
    #{'A 'b 'C 'd}
    #{'A 'b 'C 'D}
    #{'A 'B 'c 'd}
    #{'A 'B 'c 'D}
    #{'A 'B 'C 'd}})

;; aaaand back
(map-indexed vector "00-1")
(filter #(not= (second %) \-) (map-indexed vector "00-1"))
(map (clojure.set/map-invert conversion)
     (filter #(not= (second %) \-) (map-indexed vector "00-1")))

(defn to-set [coll]
  (into #{} (map (clojure.set/map-invert conversion)
                 (filter #(not= (second %) \-) (map-indexed vector coll)))))

(defn to-sets [coll]
  (into #{} (map to-set coll)))

(defn __ [funcs]
  (let [func-map (to-funcmap funcs)
        miniterms (apply concat (keys func-map))
        implicants (step (group-by #(num-ones (second %)) func-map) #{})]
  (to-sets (find-essentials miniterms implicants))))
(__  
  #{#{'a 'B 'C 'd}
    #{'A 'b 'c 'd}
    #{'A 'b 'c 'D}
    #{'A 'b 'C 'd}
    #{'A 'b 'C 'D}
    #{'A 'B 'c 'd}
    #{'A 'B 'c 'D}
    #{'A 'B 'C 'd}}) ; #{#{C B d} #{A c} #{A b}}
    
;; Test:
(= (__ #{#{'a 'B 'C 'd}
         #{'A 'b 'c 'd}
         #{'A 'b 'c 'D}
         #{'A 'b 'C 'd}
         #{'A 'b 'C 'D}
         #{'A 'B 'c 'd}
         #{'A 'B 'c 'D}
         #{'A 'B 'C 'd}})
   #{#{'A 'c} 
     #{'A 'b}
     #{'B 'C 'd}}) ; true
(= (__ #{#{'A 'B 'C 'D}
         #{'A 'B 'C 'd}})
   #{#{'A 'B 'C}}) ; true
(= (__ #{#{'a 'b 'c 'd}
         #{'a 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'B 'c 'D}
         #{'A 'B 'C 'd}
         #{'A 'B 'C 'D}
         #{'A 'b 'C 'd}
         #{'A 'b 'C 'D}})
   #{#{'a 'c}
     #{'A 'C}}) ; true
(= (__ #{#{'a 'b 'c} 
         #{'a 'B 'c}
         #{'a 'b 'C}
         #{'a 'B 'C}})
   #{#{'a}}) ; true
(= (__ #{#{'a 'B 'c 'd}
         #{'A 'B 'c 'D}
         #{'A 'b 'C 'D}
         #{'a 'b 'c 'D}
         #{'a 'B 'C 'D}
         #{'A 'B 'C 'd}})
   #{#{'a 'B 'c 'd}
     #{'A 'B 'c 'D}
     #{'A 'b 'C 'D}
     #{'a 'b 'c 'D}
     #{'a 'B 'C 'D}
     #{'A 'B 'C 'd}}) ; true
(= (__ #{#{'a 'b 'c 'd}
         #{'a 'B 'c 'd}
         #{'A 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'B 'c 'D}
         #{'A 'B 'c 'D}})
   #{#{'a 'c}
     #{'B 'c}}) ; true
(= (__ #{#{'a 'B 'c 'd}
         #{'A 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'b 'C 'D}
         #{'A 'b 'c 'D}
         #{'A 'b 'C 'D}
         #{'a 'B 'C 'd}
         #{'A 'B 'C 'd}})
   #{#{'B 'd}
     #{'b 'D}}) ; true
(= (__ #{#{'a 'b 'c 'd}
         #{'A 'b 'c 'd}
         #{'a 'B 'c 'D}
         #{'A 'B 'c 'D}
         #{'a 'B 'C 'D}
         #{'A 'B 'C 'D}
         #{'a 'b 'C 'd}
         #{'A 'b 'C 'd}})
   #{#{'B 'D}
     #{'b 'd}}) ; true
;; Okay, looks good.