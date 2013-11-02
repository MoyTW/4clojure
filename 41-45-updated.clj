; 41-45, on which I spend an inordinate amount of time messing around the
; margins of formatting for little real gain.

; 41 - Drop Every Nth Item
; Original:
(fn [v n]
  (loop [v v, n n, i 0, e (int (/ (count v) n))]
    (if (= i e) v
      (let [n-c (- (* n (inc i)) i)]
	    (if (= (count v) n-c) (subvec v 0 (dec n-c))
          (recur (apply conj (subvec v 0 (dec n-c)) (subvec v n-c)) 
		         n 
			     (inc i) 
			     e))))))
;   Ouch, that's...definitely ugly. Very, very ugly.
;   Right, I could talk about this, but this is actually so ugly it makes it
; hard to read, which is the worst kind of ugly, so we'll go with the
; equiv-algorithm revision before the explanation, rather than after.
; This will be a few steps. I'll go slow because I'm having trouble reading it,
; too.

;   Okay, first, we don't need to pass n and e into the loop/recur. While we're at
; it, we may as well rename everything. 
;   Renaming v, the input vector, to in-vec. 
;   e is the "end" number to remove, so it's n-to-rm.
;   n-c is the next position to drop. That one's complicated, but we'll put it as
;   nxt-drp. It's a little longer but, hey.
(fn [in-vec n]
  (let [n-to-rm (int (/ (count in-vec) n))]
    (loop [v in-vec, i 0]
      (if (= i n-to-rm) v
        (let [nxt-drp (- (* n (inc i)) i)]
          (if (= (count v) nxt-drp) (subvec v 0 (dec nxt-drp))
            (recur (apply conj (subvec v 0 (dec nxt-drp)) (subvec v nxt-drp)) 
                   (inc i))))))))

;   Next thing. See that 
(if (= (count v) nxt-drp) (subvec v 0 (dec nxt-drp) ) ... ) 
; statement? Yeah, well, funny thing, that's butlast. Also, the recur line is
; running over 80, so:
(fn [in-vec n]
  (let [n-to-rm (int (/ (count in-vec) n))]
    (loop [v in-vec, i 0]
      (if (= i n-to-rm) v
        (let [nxt-drp (- (* n (inc i)) i)]
          (if (= (count v) nxt-drp) (butlast v)
            (recur (apply conj (subvec v 0 (dec nxt-drp)) 
                               (subvec v nxt-drp)) 
                   (inc i))))))))
;   You know, I did have a general guideline about nested ifs and conds. Does
; it help here? Well, if cond-let were still here it would definitely, but
; apparently, it didn't get pulled up when they dropped the contrib system,
; so...outta luck. Ah, well.
;   Hmm.
;   I wonder what the process is for getting something into the core library.
(fn [in-vec n]
  (let [n-to-rm (int (/ (count in-vec) n))]
    (loop [v in-vec, i 0]
      (let [nxt-drp (- (* n (inc i)) i)]
        (cond
          (= i n-to-rm) v
          (= (count v) nxt-drp) (butlast v)
          :else (recur (apply conj (subvec v 0 (dec nxt-drp)) 
                                   (subvec v nxt-drp)) 
                       (inc i)))))))
;   So it turns out...this isn't really much better in terms of size. It's a
; little less confusing, though maybe the usage of cond over nested if
; doesn't buy a  lot of clarity. Pulling those two vars out of the loop/recur
; def is definitely good, though.
;   Yeah, I may have just spent an inordinate of time fussing about
; formatting - really the only crucial one was not having a loop/recur with
; four parameters, two of which are unneeded.

;   Anwyays, all formatting aside, what is this doing? Pick your favorite
; permutation and follow along.
;   So, first, it finds the number of elements it wants to remove. You're
; going to remove (floor (/len n)) things from the sequence in total, so it
; stores that.
;   nxt-drp indicates the next index to drop. How is it calculated?

;   First, the logic behind it: Let's use [1 2 3 4 5 6 7 8] 3 as an example.
;   We're going to drop the things marked with x: [1 2 x 4 5 x 6 7]
;   So, we're going to drop index 2 and 5, right?
;   What happens after we drop index 2, though? [1 2 4 5 x 6 7]
;   Now we're dropping index...4?
;   So the second (i = 1) element to drop is n * (i+1) - i. As you remove
; each element, you have to step back one element. The increment is because i
; is zero-based.
;   That's what (- (* n (inc i)) i) is computing.

;   So, it iterates through, dropping the next number, until the number it
; wants to drop is equal to the number it dropped, and it exists. The
; subvec-combination is the actual drop at work, and the (butlast) is to
; handle the edge case of the next thing to drop being at the end, where the
; subvec thing gets weird.
;   It's hideously over-complicated, and the algorithm - while not actually
; incorrect in any (and it's not terribly inefficient, either!) is definitely
; of the style of something you might hack together in C.

; Let's see if we can't do it by transforming sequences and such.
; The first thought is that we should find the sequence of things to drop,
; and then remove that from the sequence. Well, okay, let's give that a shot.
(fn drop-every-nth [in-vec n]
  (let [n-to-rm (int (/ (count in-vec) n))
        drp-inds (take n-to-rm (iterate #(+ % n) (dec n)))
        ind-vals (into #{} (map #(get in-vec %) drp-inds))]
    (remove ind-vals in-vec)))
; I'm not sure this is really clearer (or, heck, even that it's less code!).
; What's happening here is the same calculation of the number of elements to
; remove, but then we take a sequence of the indicies into drp-inds. We map
; those indices into a set of their corresponding values, and call remove
; with the set as a predicate.

; Hmm. The thing is, I'm almost positive there's a more simple way to do this
; that doesn't involve mucking around with indices. Well, let's see. How
; about reducing over the set and saving a count? Like, so:
(fn drop-every-nth [in-vec n]
  (let [r-help
          (fn [[out-vec i] nx]
             (if (= i n) [out-vec 1]
               [(conj out-vec nx) (inc i)]))]
  (first (reduce r-help [[] 1] in-vec))))
;   ...you know, that's just...so much simpler. Conceptually, I mean - visually
; (and depending on your tastes, code-wise) it's kind of a mess, but this is
; now my preferred algorithm. Basically, it reduces over the sequence,
; counting upwards, and if you've passed n elements it drops the next one and
; resets the count. Very simple, intuitive, no hilarous math contortions or
; mapping here to here and back again. Maybe it does use a count, which isn't
; all transform-y, but you know what? It only has to go over once. I'll take
; it.

;   Actually, you know what? There's a "clojuric" way to do it - these
; problems are constructed so that that's always the case. It's a learning
; tool, after all. So, hold on while I Google this up.

;   I'm back. Turns out there are people using keep-indexed (what is that?)
; and partition-all. I can definitely see the partition-all - call that,
; mapcat to drop last item, ezpz.
;   I have no idea what keep-index is, though.
;   Well, since I've gone and said it would be easy to use partition-all into
; mapcat, let's do that.
(fn drop-every-nth [in-vec n]
  (mapcat #(if (= (count %) n) (butlast %) %) (partition-all n in-vec)))
;   Like I said, ezpz.

; 42 - Factorial Fun
; Original:
#(reduce * (range 1 (inc %)))
;   "Write a function to calculate factorials" is one of the 'basic' CS
; questions, right up there with the Towers of Hanoi and Fibonacci numbers.
; Clojure has a 1-liner for it. Nifty, eh? This just grabs everything from 1
; to the end and multiplies it together. Viola, factorial!
;   You can do it a lot of other ways but a 1-liner wins, so these other
; ways? Not worth discussing.

; 43 - Reverse Interleave
; Original:
(fn [coll n]
  (reduce 
    (fn [sub-seqs, next-v]
      (conj (subvec sub-seqs 1) (conj (first sub-seqs) next-v))) 
	(nth (iterate #(conj % []) []) n) 
	coll))

; No-meaning-lost rewrite:
(fn rev-inter [coll n]
  (reduce
    (fn [[first-sub & rest-subs], next-v]
      (conj (into [] rest-subs) (conj first-sub next-v)))
    (into [] (repeat n []))
    coll))

;   Okay, so, this is kind of weird. It's doing a reduce, but it's being very
; strange about it. Let's break it down.
;   The reduce function takes a sequence of vectors (and the next member).
; What it does is adds the new value to the first subvector, and then places
; that subvector at the end of the sequence. As it's repeatedly called, it
; cycles through all of the subvectors in turn, ending up with a
; reverse-interleave.
;   The little thing I did there with putting the rest of the subvectors into
; a vector is so that the conj works properly and appends it to the end, not
; the front.

;   I'm trying to think of an improved algorithm, but vector/sequence issues
; aside, I actually kind of like this one. It goes over once, if fairly
; simple once you understand what the fancy footwork in the reduce function
; does...
;   Hmm. No, I'll leave that as it is.
;   ...I wonder what the 'intended' answer is, though? Hold on.

; omg so
#(apply map list (partition-all %2 %1))
; hahahahaha
;   Wow, okay. So. The partition-all breaks it up into n different sequences.
; The apply map list pulls those sequences out of the shell, and calls map
; with the n different sequences. With the predicate being list...wow.
; I am in awe.

; 44 - Rotate Sequence
; Original:
(fn rotate [r coll]
  (loop [r r coll (apply vector coll)]
    (if (= 0 r) coll
	  (if (> r 0)
	    (recur (dec r) (conj (subvec coll 1) (first coll)))
		(recur (inc r) (conj (butlast coll) (last coll)))))))
;   You know, I could rewrite this to use cond. But, nah, I don't really think
; it's worth. 'sides I'm'a do something that makes that unneeded in a second.
;   So, what this here is doing is turning the collection to the vector, and
; then either taking the front item and putting it in back, or taking the
; back item and putting it in front. It does this until it's zeroed out the
; counter it uses to keep track of how many it needs to swap.

;   Now, the thought occurs that a negative loop is also a positive loop of
; magnitude length+shift. Or, actually, it rolls over. If length is 6, -2 = 4
; = 10 - we can mod it to eliminate the negatives. So we could actually do:
(fn rotate [x coll]
  (loop [r (mod x (count coll))
         v (into [] coll)]
    (if (= 0 r) v
      (recur (dec r) (conj (subvec v 1) (first v))))))
      
;   Still, the loop thing? Eeeh. Why not just grab the chunk of n things and
; do it that way?
(fn rotate [x coll]
  (let [n (mod x (count coll))
        [first-sec last-sec] (split-at n (into [] coll))]
    (concat last-sec first-sec)))
; This is satisfactory to me. Much better than the original!

; 45 - Intro to Iterate
; Original:
[1 4 7 10 13]
; Another introduction one; nothing to be said for my solution.