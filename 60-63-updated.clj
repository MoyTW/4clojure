;;; 60-63 - In Which There Are Three One-Liners

;;; 60 - Sequence Reductions
;; Original:
(fn cust-reductions
  ([f coll] (cust-reductions f (first coll) (next coll)))
  ([f init coll]
    (if (seq coll)
      (cons init 
	      (lazy-seq (cust-reductions f (f init (first coll)) (next coll))))
	    (cons init (lazy-seq '())))))
;; Ah, lazy sequences. Lazy sequences are sometimes difficult for me to get my head around, but I seem to have done all right here. We have one of my rare usages of traditional recursion, utilizing cons, no less!
;; As a quick refresher on how lazy-seq works, what it does is it tells Clojure to only evaluates it once, and then it leaves off the evaluation until something asks for it. Also? I have no idea why I put the lazy-seq inside of the cons. It would work just as well in front, as demonstrated below:
(fn cust-reductions
  ([f coll] (cust-reductions f (first coll) (next coll)))
  ([f init coll]
    (if (seq coll)
        (lazy-seq 
		  (cons init 
		        (cust-reductions f 
				                 (f init (first coll)) 
								 (next coll))))
	    (lazy-seq (cons init '())))))
;; It doesn't *really* matter which of the two you ways you put it, it's still going to be lazy, it just modifies how many times it's run at the start. Nobody will really care either way.
;; So! What is the algorithm, here? Basically, we recurse downwards, and at each step we cons the existing value with our new generated value, found by calling the reductions function on the existing value and the next value from the collection. That's...a confusing statement - I think it's actually much clearer to simply look at the code, in this case.
;; As for algorithmic efficiency...eh. It ain't amazing. It's not particularly slow but it isn't tail recursive. I favor tail recursion heavily because of the mostly hypothetical concern that the stack might pop. Can we modify this to keep to one frame?
;; ...I, uh, probably? Hmm. You know what? I can't actually think of a way in which I could accomplish this. Let's consult The Internets.
;; Okay, just consulted. Turns out that all the situations I found either didn't work or followed roughly the same approach - recursively. So, well, if there is a way to do it like I wanted...

;;; 61 - Map Construction
;; Original:
(fn cust-zipmap [k v]
  (apply assoc {} (interleave k v)))
;; So, basically what's goin on here is that it uses interleave to pair the values after their keys in a single sequence. The sequence is then assoc'd into an empty map. It's a one-liner (or two-liner, depending on preference) so I think that's about as good as it gets!

;;; 62 - Re-implement Iterate
;; Original:
(fn cust-iter [f x]
  (cons x (lazy-seq (cust-iter f (f x)))))
;; Another one-liner! If it's a one-liner, well, that's pretty much it, isn't it? Indeed it is. This one just calls itself lazilly, applying the function as it goes. I don't think there's anoter way that's better so...as-is!

;;; 63 - Group a Sequence
;; Original:		
(fn cust-group [f s]
  (loop [f f s s m {}]
    (if (empty? s) m
	    (let [f-s (first s)]
	      (recur 
	        f 
		      (rest s)
		      (assoc m (f f-s) (conj (get m (f f-s) []) f-s)))))))
;; Okay, well, first things first. The formatting got messed up real bad, so let's fix that and do a little refactoring. Apparently, at this point I hadn't yet fully comprehended how closures work:
(fn cust-group [f s]
  (loop [s s mp {}]
    (if-let [[f-s & more] (seq s)]
	  (recur (rest s) 
	         (assoc mp (f f-s) (conj (get mp (f f-s) []) f-s)))
	  mp)))
;; So, that's kind of ugly, and it's not actually shorter in terms of lines, but it's definitely much, much cleaner.
;; The algorithm is actually very simple. Loop over the target sequence, and for each of the elements, resolve (f element) and stick that as they key into the map. In fact, I have no idea why this isn't a reduce, like so:
(fn cust-group [f coll]
  (reduce #(assoc %1 (f %2) (conj (get %1 (f %2) []) %2)) {} coll))
;; And that's only one line. Albeit, a confused and kind of convoluted one line. Expand to taste, but I like this solution!