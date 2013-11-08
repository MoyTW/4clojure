;;;; 46-52, in which I churn through a lot of very tiny problems!

;;; 46 - Flipping out
;; Original:
(fn flip [f]
  (fn ([x y] (f y x))))
;; There's not a ton to say here. It's a function which takes a function and then returns a function which calls the original function, but with the parameters flipped.
;; I guess the noteworthy lesson here is that you can compose functions! Pity it didn't stick, because I'm pretty sure that's not a thing I do very often from here on.

;;; 47 - Contain yourself
;; Original:
4
;; This is an introduction to contains?. contains? is a weird little beast because it works like you would expect on maps and sets, but when you use it with anything else it gets really weird. It actually tests for indices, so it works more like "has this many elements?" for vectors, and will always return false when called on lists.
;; Basically, never use contains? with vectors or lists, just use it with maps and sets.

;;; 48 - Intro to some
;; Original:
6
;; Some is basically "Does anything in this collection match this predicate?" Useful for, funnily enough, what you would expect contains? to do - testing for membership. Instead of "Does this collection contain :a?" it'd be like "Does anything in this collection equal :a?" instead.
;; Funnily enough, I tend to end up just filtering for something and checking whether I get any results. That's a suboptimal method that I should avoid!

;;; 49 - Split a sequence
;; Original:
(fn [x coll]
  (loop [n x in-coll coll out-coll []]
    (if (= n 0)
	  (conj [out-coll] in-coll)
      (recur (dec n) (subvec in-coll 1) (conj out-coll (first in-coll))))))
;; Oh thank you it's something that's not a one-liner. This is a rather suboptimal way of splitting a sequence in two. What it's doing is walking through a set, decrementing the counter. Each step it takes, it puts the next element into the "out" collection. Once it drops the count to zero, it takes everything past the current one and puts it on the end of the "out" collection.
;; Uh, so, you can actually just do
(fn hahaha [x coll]
  [(take x coll) (drop x coll)])
;; So, yeah.

;;; 50 - Split by Type
;; Original:
(fn split [coll]
  (vals (reduce
         (fn [m n] (assoc m (type n) (conj (get m (type n) []) n)))
         {}
         coll)))
;; Okay, so, what this is doing is basically mapping each value to its type, and then taking only the values. Simple, right? Right.
;; So, I was reading Jay Fields' blog earlier today, and he noted that when he started, he used reduce whenever he wanted to put things into maps. Then he noted a different option. Let's try that.
(fn split [coll]
  (vals 
    (apply merge-with concat 
      (map (partial apply hash-map) 
        (map (juxt type (fn [x] [x])) coll)))))
;; Okay that's just abominably ugly.
(fn split [coll]
  (->> coll
       (map (juxt type vector))
       (map (partial apply hash-map))
       (apply merge-with concat)
       (vals)))
;; Now, does that look better than my original one, or does it looks worse? It's certainly doing a lot more - jumping through hoops, as it were. I think I like my first version better - although, it could use a little reformatting; I'd break up that anonymous function, or maybe put it in a let or letfn.

;;; 51 - Advanced Destructuring
;; Original:
[1 2 3 4 5]
;; Well, it's destructuring. Destructuring is pretty awesome. Uh, that's pretty much it I guess?

;;; 52 - Intro to Destructuring
;; Original
[c e]
;; Uh, did whoever did these two get them transposed? I'm not going mad, there really is the intro *after* the advanced...right?