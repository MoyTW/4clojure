;;;; 76 - Intro to Trampoline
;; Original:
; Here is what is happening:
; foo is called with [] 1, returns #(bar [1] 1)
; #(bar [1] 1) returns #(foo [1] 3)
; #(foo [1] 3) returns #(bar [1 3] 3)
; #(bar [1 3] 3) returns #(foo [1 3] 5)
; and so on and so on until (> (last [x] 10))
[1 3 5 7 9 11]

;; So, those comments up there? They were in the original answer. This is trampoline, and it screwed with my head, and then I never used it, but it's probably good to know. If I ever happen to write something that's mutually recursive, I'll remember that trampoline can help me with that.
;; ...it's not terribly likely that I'll write a pair of functions that are mutually recursive, but I guess it's good to know anyways?

;;;; 77 - Anagram Finder
;; Original:
(fn anag [coll]
  (reduce #(conj %1 %2) #{}
          (filter #(> (count %) 1)
                  (vals ((fn to-map [coll] 
                           (reduce 
                            (fn [m n]
                              (let [k (apply conj #{} n)] 
                                (assoc m k (conj (get m k #{}) n))))
                            {} 
                coll)) 
             coll)))))
;; Uuuuhm what's going on here? Hold on a moment, let me try and reformat this, because it's...confusing, as is:
(fn anag [coll]
  (reduce #(conj %1 %2)
          #{}
          (filter #(> (count %) 1)
                  (vals ((fn to-map [coll] 
                           (reduce (fn [m n]
                                     (let [k (apply conj #{} n)] 
                                       (assoc m k (conj (get m k #{}) n))))
                                   {}
                                   coll)) coll)))))
;; That didn't help much! Okay, there are a couple of functions that are free-floating inside of my anag function. Okay, what's the one inside the reduce doing? Well, it takes the map and the next element. It generates a key, consisting of the set of the components of the next element, and then assocs that set with the next element itself.
;; So, the reduction over the collection of strings will then turn it into a map of the strings, indexed by their...sets of characters. Okay, hold on. Problem here. Sets lose data! So, this algorithm would conclude that "ayyam" is an anagram of "ayam" when in fact this is completely not true - right? Gimme a minute to fire up the repl and shove this thing in...
;;   Guess what? Try running the following (after you def anag, of course):
(anag ["ayyam" "ayam"])
;;   This returns:
#{#{"ayam" "ayyam"}}
;; The algorithm believes that these two are anagrams, but obviously they aren't - the number of letters is off! So, right away, we've gone and found an error in the algorithm...well, that's not unexpected; the code was written to past the test, after all...
;; Let's step back for a moment and exame what it is the code is doing. Basically, you map each string to the set of its component characters; multiple strings with the same set of characters are stored into a set. Then, the values of the map are taken and filtered for those containing multiple words; this collection is finally reduced into one set.
;; Right away, there are a few places we can improve on this. First off - we don't need that reduce. We can just do (into #{} ...) instead. Also, I'm...not sure why I'm bothering to define and then call to-map when it doesn't actually add any value over just calling reduce. So, we can get rid of that. Okay, where does that leave us?
(fn anag [coll]
  (into #{} 
        (filter #(> (count %) 1)
                (vals (reduce (fn [m n]
                                (let [k (apply conj #{} n)] 
                                  (assoc m k (conj (get m k #{}) n))))
                              {}
                              coll)))))
;; Ah, much better than the original! Unfortunately it still uses the same flawed algorithm so rather than continuing to refine it, first, let's step back and figure out how we might change it to be more correct. The issue at hand is that when we convert a string to a set of characters, we lose data! So, we'll have to use a more appropriate data storage form for our matching. What might we use?
;; Well, the data we want to retain is a) what characters show up and b) how often each character appears. So, a map, with the counts of each character indexed by character.
;; Fortunately, we don't have to build that ourselves because there's a handy dandy function, frequencies, that does exactly that for us.
(fn anag [coll]
  (into #{}
        (filter #(> (count %) 1)
                (vals (reduce (fn [m n]
                                (let [k (frequencies n)]
                                  (update-in m [k] (fnil conj #{}) n)))
                              {}
                              coll)))))
;; Another change I made was I searched for an alternative for that really very clumsy assoc code. update-in would theoretically be better, but the thing is we have to handle the default case of it not being in the map already with fnil, and that pretty much wipes out the conciseness or readability gains we might get by using it. On the other hand, it's always good to learn new ways to do things, so I guess that'd be a wash?
;; We could in theory further compress the function by shoving the reduce function into the # syntax, like so:
(fn anag [coll]
  (into #{}
        (filter #(> (count %) 1)
                (vals (reduce #(update-in %1 [(frequencies %2)] (fnil conj #{}) %2)
                              {}
                              coll)))))
;; but I'm not really sure that's a whole lot more readable than it was before, so it's another take-it-or-leave-it change. I wonder what the "canonical" version is supposed to be?

;; Okay, so, I did a quick Google search and most of the responses use group-by. Huh. Well. Now I feel foolish for not carefully investigating all options in the standard libraries! Observe:
(fn anag [coll]
  (into #{}
        (filter #(> (count %) 1)
                (map #(into #{} %)
                     (vals (group-by frequencies coll))))))
;; There's a lot of messing around here to get it into sets, though. Hmm. Well, group-by will always return vectors, and...yeah, I'm not sure of a much more elegant way to get these things into sets, so ugly as this may be, I think...
;; Hey, guess what. You can actually use (set ...) instead of (into #{} ...), like so:
(fn anag [coll]
  (set (filter #(> (count %) 1)
                (map set (vals (group-by frequencies coll))))))
;;   Actually, hold on, we can thread this, can't we?
(fn anag [coll]
  (->> (group-by frequencies coll)
       (vals)
       (map set)
       (filter #(> (count %) 1))
       (set)))
;; That's a pretty darn pretty threading form! I have a terrible habit of trying to squish everything onto one line, but this is pretty good, too.

;;;; 78 - Reimplement Trampoline
;; Original:       
(fn cust-tramp [f & args]
  (let [result (apply f args)]
    (loop [f result]
      (if (fn? f)
          (recur (f))
          f))))
;; This seemed really, really intimidating until I realized that, wait, all you have to do is just check if the result's a function and if so, call it.