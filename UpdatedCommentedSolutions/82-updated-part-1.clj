;;;; 82 - Word Chains - Part 1
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/82-wip.clj
;; Original:
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
;;   So, that's a pretty darn monstrous function there. It can be decomposed into
;; several functions, which would make it much easier to read. There are two
;; major parts to this. First, we need to find out which words are one step away
;; from each other. Then, when we find out which words are one step away from
;; each other, we need to figure out how to arrange them.
;;   Let's break this up, and take in two stages. First, let's examine the first
;; half - the code which maps each word to all the words that it's one step away
;; from. Next update, Part 2, will concern how to turn those sets into a chain,
;; or how to tell if no such chain exists.

;;   Converting the words to a map of words linking to words which are one step
;; away is done by the functions defined in the letfn:
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
  ...)
;;   An astute reader who also has some knowledge of string processing might note
;; that we're basically looking for strings with a levenshtein distance
;; (http://en.wikipedia.org/wiki/Levenshtein_distance) of one. I implemented
;; that here, in problem 101
;; (https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/101-wip.clj), and
;; we could pretty much copy that for a greatly reduced code footprint here, but
;; let's not at the moment. First, let's pick through what all these functions
;; are doing.

;; First, diff-by-del:
(diff-by-del [long-w short-w]
  (loop [n (count long-w)]
    (cond
      (= n 0) false
      (= (str (subs long-w 0 (dec n)) (subs long-w n)) short-w) true
    :else (recur (dec n)))))
;;   Okay. First, what in the world is this loop/recur doing in diff-by-del,
;; which, I, wait, what is going on here. What in the world? Oh,
;; that's...actually doing...something sane. Huh! Okay, what it's doing is it's
;; going through the longer word by index, basically, and at each index it rips
;; that letter out. If ripping it out causes the longer word to be equal to the
;; short one, it returns true; if it reaches index 0 it means that after
;; comparing each ripped-out string with the shorter word they weren't the same,
;; so it's false.
;;   Funnily enough, if you replace diff-by-del with the following code:
(diff-by-del [_ _] true)
;;   it still actually passes all the tests! But if you run it with the set
;; #{"share" "hares" "shares" "haze" "are"} it says they can be arranged into a
;; word chain - but the old version with the full diff-by-del correctly says
;; that the set cannot be chained. So, that's a funny coincidence there.
;;   I'm not really going anywhere with this, just noting it.
;;   So, an alternative way to find out whether they diff that doesn't use
;; loop/recur (because we should strive to use higher-level constructs if
;; possible) would be something like "Generate all the substrings of the longer
;; word; if you can find in those the shorter word, true, else false. So...let's
;; say our word is "zeus".
;;   "zeus" -> #{"eus" "zus" "zes" "zeu"}
(map #(str (subs "zeus" 0 %) (subs "zeus" (inc %) (count "zeus"))) (range (count "zeus")))
;;   It's doing the same substring generation technique, but this is a mapping.
;; Let's wrap it up:
(diff-by-del [long-w short-w]
  (contains? (set (map #(str (subs long-w 0 %) (subs long-w (inc %) (count long-w)))
                       (range (count long-w))))
             short-w))
;;   Okay, I'll admit that the map syntax there is crowded and terrible. Let's
;; see...how to make it a little more readable...?
(diff-by-del [long-w short-w]
  (->> (range (count long-w))
       (map #(str (subs long-w 0 %) (subs long-w (inc %) (count long-w))))
       (some #(= % short-w))))
;;   Not sure if it's better or worse or what, but I do like threading macros.

;; Now, diff-by-sub:
(diff-by-sub [l-w r-w]
  (if (= 1 (reduce #(if %2 %1 (inc %1)) 
                   0 
                   (map #(= %1 %2) l-w r-w)))
    true
    false))
;;   That's kind of convoluted, actually. What it's doing is mapping the two
;; words (we assume these two words are of equal length) to a sequence of
;; true/falses. Hold on, let me rewrite this, there's a ton of cruft here:
(diff-by-sub [l-w r-w]
  (= 1 (reduce #(if %2 %1 (inc %1)) 
               0
               (map = l-w r-w))))
;;   I really don't like that #(if %2 %1 (inc %1)), that's just...ugh.
(diff-by-sub [l-w r-w]
  (= 1 (count (remove identity (map = l-w r-w)))))
;;   Okay, that's acceptably concise and reasonably clear. Let's move on.

;; Now, diff-by-one:
(diff-by-one [l-w r-w]
  (let [l-cnt (count l-w) r-cnt (count r-w)]
    (cond 
      (= l-cnt r-cnt) (diff-by-sub l-w r-w)
      (= (inc l-cnt) r-cnt) (diff-by-del r-w l-w)
      (= (inc r-cnt) l-cnt) (diff-by-del l-w r-w)
      :else false)))
;;   What this function does it checks the following conditions:
;;     * If equal lengths, will a substitution work?
;;     * If left is one greater than right, will a deletion work?
;;     * If right is one greater than left, will a deletion work?
;;     * Else false
;;   Actually, looking at this, I think it's fine as it is. It could be make more
;; concise or restructured, but the methods to restructure it that I'm thinking
;; of - maybe run them all, and then if any of them is true, return true, else
;; false? seem to be much more complex for little gain. Also I have an
;; inordinate fondness for the (cond) structure, so there's that.
;;   The only real issue I have here is l-w r-w l-cnt r-cnt oh dear words. Oh!
;; And we can do away with :else false; that's implicit.
(diff-by-one [left right]
    (cond (= (count left) (count right)) (diff-by-sub left right)
          (= (inc (count left)) (count right)) (diff-by-del right left)
          (= (inc (count right)) (count left)) (diff-by-del left right)))

;; Now find-one-shifts:
(find-one-shifts [wrd st]
  (filter #(diff-by-one wrd %) st))
;;   This function takes a word and the set of all words and returns words which
;; have a distance of one from that word. Simple.

;; Finally, set-to-sorted-map:
(set-to-sorted-map [s]
  (let [unsorted (reduce #(assoc %1 %2 (find-one-shifts %2 s)) {} s)]
    (into (sorted-map-by #(compare [(count (get unsorted %1)) %1]
                                   [(count (get unsorted %2)) %2]))
          unsorted)))
;;   This function converts the set into a sorted map, with the keys as the words
;; and the values as the sets of all words with a distance of one. The sort
;; criteria is the length of the key. The reason I wanted it sorted was because
;; of the algorithm I was using to detect word chains - it matched the words
;; with the least possible chains first.
;;   The algorithm isn't assured to produce good outcomes, however, and the fact
;; that the unit tests had nothing in there that broke it doesn't mean it's any
;; good.
;;   I have no particular beef with this one specific code sample, but in the
;; very near future (next update) we'll be looking at the algorithm for chaining
;; - and the new algorithm won't be needing sorting. So we can cut
;; set-to-sorted-map down to the following:
(set-to-sorted-map [s]
  (reduce #(assoc %1 %2 (find-one-shifts %2 s)) {} s))
  
;; We're halfway through, and our new code looks like this:
(letfn [(diff-by-del [long-w short-w]
          (->> (range (count long-w))
               (map #(str (subs long-w 0 %) (subs long-w (inc %) (count long-w))))
               (some #(= % short-w))))
        (diff-by-sub [l-w r-w]
          (= 1 (count (remove identity (map = l-w r-w)))))
        (diff-by-one [left right]
          (cond (= (count left) (count right)) (diff-by-sub left right)
                (= (inc (count left)) (count right)) (diff-by-del right left)
                (= (inc (count right)) (count left)) (diff-by-del left right)))
        (find-one-shifts [wrd st]
          (filter #(diff-by-one wrd %) st))
        (set-to-sorted-map [s]
          (reduce #(assoc %1 %2 (find-one-shifts %2 s)) {} s))]
  ...)
;;   Note that, because of the changes we've made to the set-to-sorted-map
;; function it's not actually sorted, so if you try to paste this into the old
;; code which passes the tests it won't work! You'll have to un-replace
;; set-to-sorted map to do that.

;;   Of course, we could replace the find-one-shifts with this code, from the far
;; and distant future:
(fn lev-dist [left right]
  (let [cost (if (= (last left) (last right)) 0 1)]
    (cond 
      (= 0 (count left)) (count right)
      (= 0 (count right)) (count left)
      :else (min (inc (lev-dist (butlast left) right))
                 (inc (lev-dist left (butlast right)))
                 (+ cost (lev-dist (butlast left) (butlast right)))))))
;;   integrated as so:
(letfn [(lev-dist [left right]
          (let [cost (if (= (last left) (last right)) 0 1)]
            (cond (= 0 (count left)) (count right)
                  (= 0 (count right)) (count left)
                  :else (min (inc (lev-dist (butlast left) right))
                             (inc (lev-dist left (butlast right)))
                             (+ cost (lev-dist (butlast left) (butlast right)))))))
        (find-one-shifts [wrd st]
          (filter #(= 1 (lev-dist wrd %)) st))
        (set-to-sorted-map [s]
          (reduce #(assoc %1 %2 (find-one-shifts %2 s)) {} s))]
  ...)
;;   Do note that this particular implementation of the algorithm is naive and if
;; you have words of any significant length it will take a very long time to
;; process. Fortunately our sample set is composed of rather short words, but
;; it's something to keep in mind.

;;   Next time, on Part 2, we chain! Or, rather, verify the possibility of
;; chaining; it's not technically required that we do it.