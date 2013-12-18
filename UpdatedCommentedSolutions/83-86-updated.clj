;;;; 83 - A Half-Truth
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/83-wip.clj
;; Original:
(fn long-xor [& args]
  (if (= (count (apply conj #{} args)) 2) true false))
;;   This is a very simple chained XOR operation. The above code snippet shoves
;; the arguments into a set, and then checks to see whether the set is of size 2
;; - if it's of size 2, there must be a true and a false, and therefore we
;; return true. Otherwise, false.
;;   This will of course fail if anything other than booleans are passed in, but
;; the problem specifically says "a variable number of booleans" so we're good.
;;   Doesn't mean we can't gold this around a bit. First off, that if? It...does
;; nothing. So let's get rid of that. Also, (apply conj set args) could just be
;; (set args). We could also do something like check that it's equal to #{true
;; false} or check for the presence of true and false in the set of args. You
;; can mix and match:
(fn long-xor [& args]
  (= (count (set args)) 2))
;;   or
(fn long-xor [& args]
  (= #{true false} (set args)))
;;   or a couple of other ways; I think the set comparison is the clearest, and
;; the count comparison is the shortest I can come up with off the top of my
;; head, but it's almost trivial, so let's move on.

;;;; 84 - Transitive Closure
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/84-wip.clj
;; Original:
(fn closure [st]
  (let [mp (reduce #(assoc %1 (first %2) (second %2)) {} st)]
    (reduce
      (fn close-over [out-set next-key]
        (loop [os out-set k (first next-key)]
          (let [v (get mp k)]
            (if (or (= k nil) (= v nil)) os
              (recur (conj os [(first next-key) v]) v)))))
      #{} mp)))
;;   That syntax is...pretty tortured. Before I do anything else, let me very
;; quickly reformat this:
(fn closure [st]
  (let [mp (reduce #(assoc %1 (first %2) (second %2)) {} st)]
    (reduce (fn close-over [out-set next-key]
              (loop [os out-set k (first next-key)]
                (let [v (get mp k)]
                  (if (or (= k nil) (= v nil)) 
                      os
                      (recur (conj os [(first next-key) v])
                             v)))))
            #{}
            mp)))
;;   For the benefit of my ability to read the code, I've gone and done some
;; minor restructuring. Nothing that changes the meaning, mind.
(fn closure [st]
  (let [mp (into {} st)]
    (reduce (fn close-over [out-set [key _]]
              (loop [os out-set k key]
                (let [v (get mp k)]
                  (if (or (= k nil) (= v nil)) 
                      os
                      (recur (conj os [key v])
                             v)))))
            #{}
            mp)))
;;   So, most of the logic happens in the function close-over, which takes a
;; key/value pair, and appends the closure chain (if that's a term) onto the
;; output set. It does this by essentially recursing down the chain until it
;; reaches the end; the loop/recur takes the set, and the next key in the chain.
;; Maybe that sounds unclear (actually, I know it does) so let's use an example.
;;   Take the following input:
#{["cat" "man"] ["man" "snake"] ["spider" "cat"]}
;;   When we turn it into a map, it becomes:
{"cat" "man", "man" "snake", "spider" "cat"}
;;   The algorithm begins, taking in cat/man, with cat bound to key at the top.
;;   Then, it loops like the following:
;;     (loop out-set key) -> (loop #{} "cat")
;;     (recur (conj #{} ["cat" "man"]) "man") -> (loop #{["cat" "man"]} "man")
;;     (recur (conj #{["cat" "man"]} ["cat" "snake"]) "snake") 
;;       -> (loop #{["cat" "man"] ["cat" "snake"]} "snake")
;;     "snake" is not a key, function returns #{["cat" "man"] ["cat" "snake"]}

;;   So how is this algorithm? It's certainly not bad in the sense that it's
;; inefficient, because it's not. It does do "bare" recursion but sometimes
;; that's what you want. I don't actually have any objections to the algorithm!
;; I think that the way in which I put it together - especially defining it
;; inside the reduce - is obnoxious, but that's syntax stuff, we can just stick
;; it into a letfn or let or something. Actually, let's just go ahead and do
;; that:
(fn closure [st]
  (let [mp (into {} st)
        close-over (fn [out-set [key _]]
                     (loop [os out-set k key]
                       (if (or (= k nil) (= (mp k) nil)) 
                           os
                           (recur (conj os [key (mp k)])
                                  (mp k)))))]
    (reduce close-over #{} mp)))
;;   We're just messing around on the margins at this point. Time to move on.

;;;; 85 - Power Set
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/85-wip.clj
;; Original:
(fn power-set [trgt-st]
  (letfn [(break-up [st]
                    (reduce #(conj %1 (disj st %2)) #{} st))
          (next-line [st]
                     (reduce #(apply conj %1 (break-up %2)) #{} st))]
    (loop [st #{trgt-st} pwr-st #{trgt-st}]
      (if (= st #{#{}}) pwr-st
        (recur (next-line st) (apply conj pwr-st (next-line st)))))))
;;   Wow, this is really dense and unclear! Okay, what is it that's going on in
;; this function? There are a couple of helper functions, and then there's a
;; loop/recur, so let's start with the helper functions:

;; break-up:
(break-up [st]
  (reduce #(conj %1 (disj st %2)) #{} st))
;;   So, what this is actually doing is subtracting elements one by one from the
;; passed-in set. The generated sets of size (original - 1) are accumulated with
;; the reduce and returned inside of a new set. So, if we have #{:a :b :c}, the
;; output would be #{#{:b :c} #{:a :c} #{:a :b}}. Oh, I see how I solve it now -
;; we start from the top. This will generate the next layer of sets, and we
;; recurse downwards until we end up with size-one sets. Then we accumulate
;; every generated set into one set, and that's the power set.
;;   We actually don't need a reduce here - we'd be better served with a map:
(break-up [s]
  (map #(disj s %) s))
;;   That's clearer, isn't it? Also, smaller.

;; next-line:
(next-line [st]
  (reduce #(apply conj %1 (break-up %2)) #{} st))
;;   This just applies break-up to a set of sets. Again, I'm not sure the reduce
;; is the best way to go about this. How about mapcat? If we do that, though, we
;; end up with a sequence, not a set, so...
(next-line [s]
  (set (mapcat break-up s)))
;; Heck, we could just roll break-up into next-line, if we wanted.

;; Finally, the body:
(fn power-set [trgt-st]
  ...
  (loop [st #{trgt-st} pwr-st #{trgt-st}]
    (if (= st #{#{}}) pwr-st
      (recur (next-line st) (apply conj pwr-st (next-line st)))))))
;;   The loop/recur accumulates the derived sets into pwr-st. It repeatedly calls
;; next-line to break up a line of sets, and stops when next-line returns the
;; empty set (well, the set containing an empty set, actually). That's pretty
;; much it - it looks very intimidating, but the algorithm actually is quite
;; simple. Unfortunately, I couldn't think of a way to avoid the loop/recur,
;; since our end condition is "When you've got it down to no more members."
;;   Now, how would be simplify the code and make it less, uh, confusing? Okay,
;; first, pulling out the vowels makes reading things a total pain, so we can
;; start there.
(fn find-power-set [in-set]
  ...
  (loop [s #{in-set} power-set #{in-set}]
    (if (= s #{#{}}) power-set
        (recur (next-line s) (apply conj power-set (next-line s)))))))
;;   Surprisingly I don't see a lot of room for improvement here, assuming we
;; keep the algorithm. As in, I can't see anything obvious I want to change. Go
;; past me?

;;   On the other hand there is a different algorithm we could use - reduce over
;; the original set, and just add each new member to every existing member of
;; the set. That...that would actually probably end up smaller than what I've
;; got now. Still, I've got it down to a wholly acceptable six lines (though the
;; lines are pretty dense) so there's that!
(fn power-set [trgt-st]
  (letfn [(break-up [s] (map #(disj s %) s))
          (next-line [s] (set (mapcat break-up s)))]
    (loop [st #{trgt-st} pwr-st #{trgt-st}]
      (if (= st #{#{}}) pwr-st
        (recur (next-line st) (apply conj pwr-st (next-line st)))))))

;;;; 86 - Happy Numbers
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/86-wip.clj
;; Original:
(fn is-happy [n]
  (letfn [(to-chars [n] (reduce #(conj %1 (Character/getNumericValue %2)) [] (str n)))
          (happy-step [n] (reduce #(+ %1 (* %2 %2)) 0 (to-chars n)))
          (gen-happy [n] (cons n (lazy-seq (gen-happy (happy-step n)))))]
    (contains? (reduce #(conj %1 %2) #{} (take 100 (gen-happy n))) 1)))
;;   This is really just a series of one-liners all shoved together. I seem to
;; have been making a habit out of this technique! The general algorithm is as
;; follows - we generate the first hundred numbers in the sequence, and if
;; there's a 1 somewhere in there, it's happy - otherwise we just sort of assume
;; that if it were happy it'd have shown up in the first hundred of the
;; sequence, and say it's not.
;;   I'm aware that this is a very fuzzy algorithm! However, (as might be
;; observed if you look at my scratch paper), I generated sequences of various
;; numbers and 100 seems a more than reasonable number; most started repeating
;; far before that.
;;   As for the actual code, we'll take it line-by-line:

;; to-chars:
(to-chars [n] (reduce #(conj %1 (Character/getNumericValue %2)) [] (str n)))
;;   This takes a number, converts it to a string, and then converts it back to a
;; vector full of integers. This is also a place where we really should use map:
(to-chars [n] (map #(Character/getNumericValue %) (str n)))
;;   The reason we can't just do Character/getNumericValue like we normally would
;; is because it's a Java function from Character, not a native Clojure one, so
;; we have to give it a hand.

;; happy-step:
(happy-step [n] (reduce #(+ %1 (* %2 %2)) 0 (to-chars n)))
;;   This function just sums the squares of the numbers in the list generated by
;; to-chars. We can cut it down a bit, though:
(happy-step [n] (reduce + (map #(* % %) (to-chars n))))

;; gen-happy:
(gen-happy [n] (cons n (lazy-seq (gen-happy (happy-step n)))))
;;   This generates an infinite lazy sequence, according to the break-and-square
;; algorithm.

(fn is-happy [n]
  ...
    (contains? (reduce #(conj %1 %2) #{} (take 100 (gen-happy n))) 1)))
;;   This generates 100 numbers in the sequence, shoves them into a set (with a
;; reduce? really?) and then checks for 1. Hold on, this is silly, let me edit
;; it real quick:
(fn is-happy [n]
  ...
    (contains? (set (take 100 (gen-happy n))) 1)))
    
;;   There. Now, replacing the old code with the new:
(fn is-happy [n]
  (letfn [(to-chars [n] (map #(Character/getNumericValue %) (str n)))
          (happy-step [n] (reduce + (map #(* % %) (to-chars n))))
          (gen-happy [n] (cons n (lazy-seq (gen-happy (happy-step n)))))]
    (true? (some #(= % 1) (set (take 100 (gen-happy n)))))))
;;   You know what, let's roll those 3 helper functions into one:
(fn is-happy [n]
  (letfn [(gen-happy [n] (cons n
                               (lazy-seq (gen-happy (reduce +
                                                            (map #(* % %)
                                                                (map #(Character/getNumericValue %)
                                                                     (str n))))))))]
    (true? (some #(= % 1) (set (take 100 (gen-happy n)))))))
;;   That particular sloping construction is a very distinctive sign that you can
;; probably use a threading macro:
(fn is-happy [n]
  (letfn [(gen-happy [n] 
            (->> (str n)
                 (map #(Character/getNumericValue %))
                 (map #(* % %))
                 (reduce +)
                 (gen-happy)
                 (lazy-seq)
                 (cons n)))]
    (true? (some #(= % 1) (set (take 100 (gen-happy n)))))))
;;   You know I don't think that's any better. In fact, I think the original
;; updated form is much better; it's both prettier and more immediately
;; understandable. Just goes to show - sometimes threading macros are not the
;; answer.