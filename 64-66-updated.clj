;;; 64 - 66 - Wait, really!? They didn't put reduce earlier?

;;; 64 - Intro to Reduce
;; Original:
+
;; Wait, wait, wait, it took until *now* for them to get around introducing reduce? No wonder I was so loop/recur happy!

;;; 65 - Black Box Testing
;; Orignal:
(fn check-type [coll]
  (let [coll-m (conj coll {:w :x :y :z})]
	(if (= (+ 2 (count coll)) (count coll-m))
	    :map
      (let [coll-c (conj coll :z :z :x)]
        (if (= (+ 2 (count coll)) (count coll-c))
	        :set
	        (if (= (last coll-c) :x)
	          :vector
	          :list))))))
;; So, what's going on here? Well, we want to figure out what kind of collection it is - so I decided to add things, and then check where the things get added.
;; If you add a map of two elements, it will resolve as one item in all the collections but map - if there are two added, it must be a map.
;; If you add duplicate elements, but it doesn't increment an equal number of times to the elements inserted, it must be a set, because it eliminates duplicates.
;; If you add something and it's added on the end it's a vector - if it's added on the front, it's a list.
;; Funny story - the very first thought I had to check if it were a map was to try to assoc something, and if it worked it'd be a map. Well, the algorithm's kinda flawed because it would work for a certain set of key values for lists, but it would have worked - if 4Clojure didn't disable exception-related functions.
;; Back to the code! Those were the criteria I used, but this thing's a little inelegant, isn't it? So let's rewrite it some. Specifically, I'm going to use cond, because cond is perfect for this:
(fn check-type [coll]
  (cond
    (= (+ 2 (count coll)) (count (conj coll {:w :x :y :z}))) :map
    (= (inc (count coll)) (count (conj coll :z :z))) :set
    (= (last (conj coll :z :x)) :x) :vector ;Add two to handle empty case
    :else :list))
;; Now, this will mostly work - but if there's any collection tested which contains the keywords I use to test, it might get a little zany. Like if you do the following:
(check-type #{:z}) ; Assume we def'd it somewhere
;; The function thinks it's a vector! Well, that's obviously wrong. What sorcery? Oh, right, it's because we use :z to test whether it's a set.
;; Okay, we could actually plug this hole by checking >=, instead of = in the set condition, but my point stands - this is vulnerable to weird happenings if you pass in collections which contain the test keywords. I don't think that's really a problem, per se, since it's more a learning exercise than usable code, but it's something to be aware of.
						
;;; 66 - Greatest Common Divisor
;; Original:
(fn gcd [a b]
  (loop [a a b b]
    (if (= a b) a
      (if (> a b) (recur (- a b) b)
        (recur a (- b a))))))
;; So, yeah, this is Euclid's algorithm again, and it's a pretty trivial algorithm that you can go look up easily, so...this is more or less trivial. I mean, it's a little ugly, but hey! The algorithm is solid, and I'm sure I could golf it around a bit but I'll leave it as is.