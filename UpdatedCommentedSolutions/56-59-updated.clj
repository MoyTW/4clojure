;; 56-59, In Which My Own Solutions Are Pretty Nifty, If I Might Say So Myself

;;; 56 - Find Distinct Items
;; Original:
(fn cust-distinct [coll]
  (reduce
    (fn [m n]
	    (if (some #{n} m) m
		    (conj m n)))
	  []
  	coll))
;;   Well, that's kind of ugly, isn't it? Putting a function instide a reduce
;; always ends up looking a little inelegant. It basically reduces its way over
;; the set and if something's already been placed into the output list, it
;; returns the output list without adding it. If it hasn't, it adds it.
;;   The algorithm is dead simple, and doesn't have a lot of undesirable traits
;; so far as efficiency goes. Still, what are the alternative approaches?
;;   Well...no, honestly I think that's a pretty solid algorithm that fills all
;; the requirements of the problem quite well! Let's see how the Internets
;; solved it...

;; http://yyhh.org/blog/2011/05/my-solutions-problems-no-51-75-4clojure-com
(fn [coll] 
  ((fn step [[x & xs] seen] 
     (when x
       (if (seen x) 
         (step xs seen)
         (cons x (step xs (conj seen x)))))) 
   coll #{}))
;;   This is actually a very different way of solving it! Instead of reducing
;; over the sequence, this solution is recursive. It carries a set, seen, and
;; returns everything it doesn't find in the seen set one by one. I like it! I
;; think that you should probably shy away from bare recursion, barring
;; something like the requirement that you produce a lazy sequence, but actually
;; thinking recursively is a valuable skill to cultivate.

;; Out of curiosity, what's the source of distinct?
(defn distinct
  "Returns a lazy sequence of the elements of coll with duplicates removed"
  {:added "1.0"
   :static true}
  [coll]
    (let [step (fn step [xs seen]
                   (lazy-seq
                    ((fn [[f :as xs] seen]
                      (when-let [s (seq xs)]
                        (if (contains? seen f) 
                          (recur (rest s) seen)
                          (cons f (step (rest s) (conj seen f))))))
                     xs seen)))]
      (step coll #{})))
;;   Ah! This is...basically the same approach as the yyhh guy's source. Except
;; that it is indeed lazy. Well!

;;; 57 - Simple Recursion
;; Original:
[5 4 3 2 1]
;;   I'm quite curious how you'd manage to get to 57 without having *any* idea of
;; the basics of recursion. I'd have expected this to be much earlier! Other
;; than that, there's not really much to say - it's an intro to recursion.

;;; 58 - Function Composition
;; Original:
; Note that this only handles the arg cases it has to to pass the unit tests
; I think there's a "a b c" case I missed.
(fn run-in-order
  ([x] x)
  ([x y]
   (fn 
     ([arg] (x (y arg)))
     ([a b] (x (y a b)))
     ([a b c & args] (x (apply y a b c args)))))
  ([x y z]
   (fn 
     ([arg] (x (y (z arg))))
     ([a b] (x (y (z a b))))
     ([a b c & args] (x (y (apply z a b c args)))))))
;;   Ah, and here we are introduced to the wonder of functions with multiple
;; sigs. I did three, but you could go on.
;;   What it does should be fairly self-explanatory. It takes functions, and then
;; makes functions from the functions. Unfortunately it seems at the time, I
;; hadn't quite comprehended how multiple-argument functions work! I had the
;; general idea right, but I didn't have a lot of fine motor control, if it
;; were. So, we can do this:
(fn run-in-order
  ([x] x)
  ([x y]
   (fn ([& args] (x (apply y args)))))
  ([x y z]
   (fn ([& args] (x (y (apply z args)))))))
;;   Much better, but we still are limited to only three functions. Can we make
;; it allow an arbitrary number of functions? (Of course we can). So, what we
;; need to do is build a function that takes the list, and runs it right to
;; left. So...
((
(fn run-in-order [& fs]
  (fn c-comp [& args]
	(loop [funcs (reverse fs)
	       args args]
      [funcs args]))) 
rest reverse) [1 2 3 4])
;;   The above is an intermediate step. We peek into what will be passed to the
;; function. Oh, it looks like...a list of functions, and a sequence of
;; arguments. So, this should work, right?
((
(fn run-in-order [& fs]
  (fn c-comp [& args]
	(loop [funcs (reverse fs)
	       args args]
      (if (seq funcs) 
	      (recur (rest funcs) (apply (first funcs) args))
		  args))))
rest reverse) [1 2 3 4])
;;   ...why is it telling me that we're passing four things to rest? Ah! We're
;; stripping it down a level with the apply. We can fix that.
((
(fn run-in-order [& fs]
  (fn c-comp [& args]
	(loop [funcs (rest (reverse fs))
	       arg (apply (last fs) args)]
      (if (seq funcs) 
	      (recur (rest funcs) ((first funcs) arg))
		  arg))))
rest reverse) [1 2 3 4])
;;   So, what just happened is that we've gone and moved the very first call into
;; the binding. This means that we don't have to worry about fussing about with
;; apply, because we can be assured that after the first run, we have one and
;; only one arg.
;;   I quite like this one! See, it uses loop/recur, so you never have to worry
;; about overflowing things (not that you have to worry about overflowing things
;; anyways for the most part) and it makes hilarious usage of and perhaps abose
;; of closures. Also it's pretty short.
;;   And, so, feed the following into #58, and it'll come up green:
(fn run-in-order [& fs]
  (fn c-comp [& args]
	(loop [funcs (rest (reverse fs))
	       arg (apply (last fs) args)]
      (if (seq funcs) 
	      (recur (rest funcs) ((first funcs) arg))
		  arg))))
		  
;;; 59 - Juxtaposition
;; Original:
(fn cust-jux
  ([x] (fn [& args] (list (apply x args))))
  ([x y] (fn [& args] (list (apply x args) (apply y args))))
  ([x y z] (fn [& args] (list (apply x args) (apply y args) (apply z args)))))
;;   So, similarly to the above, what we have here is a multi-sig function. What
;; it actually does should be pretty obvious - returns a function which calls
;; each function on the arguments then mashes them together. It works, but it
;; caps out at three, and given that there are only three numbers - zero, one,
;; and infinity - this function is sorely lacking. So, let's rewrite this.
(fn cust-jux [& fs] (fn [& args] (map #(apply % args) fs)))
;;   Whoop whoop, a one-liner! Or, two-liner. Or, I guess you could make it three
;; if you wanted to format it that one. Actually, that'd be much more clear,
;; wouldn't it?
(fn cust-jux [& fs] 
  (fn [& args] 
    (map #(apply % args) fs)))
;;   So, all it's doing is taking that list of functions, and then using map to
;; resolve the inputs to the returned function into the list. And then you're
;; done!