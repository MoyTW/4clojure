;;;; 101 - Levenshtein Distance
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/101-wip.clj
;; Original:
(fn l-d [left right]
  (let [cache (atom {})]
    (letfn [(lev-dist [left right]
              (let [cost (if (= (last left) (last right)) 0 1)
                    key [(count left) (count right)]]
                (cond
                  (= 0 (count left))
                      (do (swap! cache assoc key (count right)) (count right))
                  (= 0 (count right))
                      (do (swap! cache assoc key (count left)) (count left))
                  (contains? @cache key) (get @cache key)
                  :else
                  (let [val
                          (min
                            (inc (get @cache key (lev-dist (butlast left) right)))
                            (inc (get @cache key (lev-dist left (butlast right))))
                            (+ cost (get @cache key (lev-dist (butlast left) (butlast right)))))]
                    (do (swap! cache assoc key val) val)))))]
      (lev-dist left right))))
;;   So, the Levenshtein distance between two strings is the minimum number of
;; deletions, insertions, and swaps which can transform one to the other. I
;; didn't go to write my own algorithm for this - the wikipedia entry
;; (http://en.wikipedia.org/wiki/Levenshtein_distance#Computing_Levenshtein_distance)
;; has a number of example algorithms for finding the desired value.
;;   The first thing that I did was do a straight implementation of the very
;; inefficient recursive solution presented by the Wikipedia article, which is
;; basically trivial in complexity but horrifyingly slow:
(defn lev-dist [left right]
  (let [cost (if (= (last left) (last right)) 0 1)]
    (cond
      (= 0 (count left)) (count right)
      (= 0 (count right)) (count left)
      :else
      (min (inc (lev-dist (butlast left) right))
           (inc (lev-dist left (butlast right)))
           (+ cost (lev-dist (butlast left) (butlast right)))))))
;;   Now, it works just fine in the repl:
(= 3 (lev-dist "kitten" "sitting"))
(= (lev-dist "closure" "clojure") (lev-dist "clojure" "closure") 1)
(= (lev-dist "" "123456") 6)
;;   Unfortunately it times out when you put this code into 4Clojure! So, in
;; order to fix this, we can cache the results (or change the algorithm, but I'd
;; never worked with mutable data in Clojure and I figured a) it'd be good to
;; experiment with and b) I didn't really want to actually go and hunt
;; down/think of a different algorithm). So, what are our options here?
;;   There are four different structures for dealing with mutable data in
;; Clojure, the ref, atom, agent, and var. There's an easy explanation here
;; (http://stackoverflow.com/questions/9132346/clojure-differences-between-ref-var-agent-atom-with-examples)
;; about what they do but basically, what we want to use here is a atom
;; (http://clojure.org/atoms) because we only have one data structure to work on
;; (our cache, represented as a map) and we also want the changes to be
;; synchronous.
;;   The cache will be a map, with the key being the
;; [number-of-characters-in-left number-of-characters-in-right] and the value
;; being the number of shifts. Whenever we want to find the distance between two
;; strings, we first check the cache. If we can't find a match in the cache, we
;; do the calculation, but before we return it we store it in the cache.
;;   So, what we need to do to retrofit it is just a few simple steps:
;;     * Add an atom:
;;         (let [cache (atom {})] ...
;;     * Add a lookup before the recursive call:
;;         (cond
;;           ...
;;           (contains? @cache key) (get @cache key)
;;           :else ...)
;;     * Whenever the function would return a value, first store it
;;         Replace (= 0 (count ...)) (count ...) in cond with:
;;           (do (swap! cache assoc key (count ...)) (count ...))
;;         Replace :else with:
;;           (let [val ...] (do (swap! cache assoc key val) val))
;;   One thing to note about my original result, as posted, is that I put the
;; lookup *under* the two "If we've consumed the entire left/right sequence"
;; conditions. This probably wouldn't be a huge performance difference, but if
;; the action after the condition were heavier than it is you'd definitely want
;; to put the check in front. Other than that, I think the code is a reasonable
;; implementation. The letfn is pretty ugly, admittedly, but unfortunately we
;; can't collapse it into the let because let doesn't support defining recursive
;; functions. Aside from those two quibbles, though, I'm content to leave this
;; one here.

;;;; 102 - intoCamelCase
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/102-wip.clj
;; Original: 
(fn camel-case [word]
  (let [[lead & words] (clojure.string/split word #"-")]
    (apply str lead (map clojure.string/capitalize words))))
;;   This is pretty trivial. Basically, you want to break it up by the '-'
;; character, then capitalize everything but the first one, and then smash them
;; together. I'm...not sure why I called the head word lead instead of head, but
;; hey.
;;   I tried thinking of some way to make this cleaner, but I think the let is
;; the best way; otherwise you'll end up having to split twice to get the first
;; and rest. Unless I'm missing a trick! If I am, though...
;;   See, you can't really thread this; apart from map having an incompatible
;; threading form with the clojure.string functions, you have to actually take
;; the first and treat it differently than the rest. It's not a fully
;; homogeneous list of strings, because of that special case. Hmm.
;;   No, I think I'll leave it. Now I'm curious, though. What's the one-liner? Be
;; right back, searching The Internets.
;;   Huh:
#(clojure.string/replace % #"-(\w)" (fn [[a b]] (clojure.string/capitalize b)))
;;   That works! More clever'n mine.