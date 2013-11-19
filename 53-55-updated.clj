;; 53-55, In Which I Consult The Internets For Interesting Solutions
;; (my solutions are boring and overly imperative)

;;; 53 - Longest Increasing Sub-Seq
;; Original:
(fn longest [coll]
  (loop [coll coll longest (seq '())]
    (if (= (first coll) nil)
	    (if (> (count longest) 1) longest [])
	    (let [next-inc-seq 
                (loop [coll coll inc-seq [-1]]
                  (if (or (= (first coll) nil) (<= (first coll) (last inc-seq)))
                      (rest inc-seq)
	                  (recur (rest coll) (conj inc-seq (first coll)))))]
	      (recur (subvec coll (count next-inc-seq))
		         (if (>= (count longest) (count next-inc-seq))
		             longest
		             next-inc-seq))))))

;; Hoo boy that there is ugly. What does it do?
;;   Well, the fact that I set notepad++ tabs to be two spaces by default really
;; screwed up the formatting. So, there's that. It's nigh-impossible to read, so
;; I'm going to reformat it some.
;;   Wow, this is tangled. Okay. So, we've got a loop/recur taking in the rest of
;; the collection and the longest increasing subsequence.
;;   It starts off with checking if the sequence has been consumed. If it has
;; been consumed, you check whether the longest increasing subseqence is greater
;; than length one; if it is, return it, otherwise return an empty subsequence.
;;   If the subseqence hasn't been consumed, we call a helper function to find
;; the next increasing sequence, and store it in next-inc-seq. We then recur
;; with the collection, minus the first (count next-inc-seq) members, and the
;; longer of the two increasing subsequences, with the first given priority.

;;   That is very complicated. Part of the complexity comes from the frankly
;; insane manner in which I've structured it; the assignment to next-inc-seq
;; should really be a helper function, there's an if way down in the parameters
;; to recur. I'm going to leave the algorithm in place but move some stuff
;; around:
(fn longest[coll]
  (letfn [(find-next-inc-seq [coll]
            (loop [coll coll inc-seq [-1]]
			  (if (or (= (first coll) nil) (<= (first coll) (last inc-seq)))
			      (rest inc-seq)
				  (recur (rest coll) (conj inc-seq (first coll))))))]
    (loop [coll coll longest (seq '())]
      (if (= (first coll) nil)
	      (if (> (count longest) 1) longest [])
	      (let [next-inc-seq (find-next-inc-seq coll)
		        longest-inc-seq
				  (if (>= (count longest) (count next-inc-seq)) 
				      longest 
				      next-inc-seq)]
	        (recur (subvec coll (count next-inc-seq))
		           longest-inc-seq))))))
;; Second stage of cleanup.
(fn longest[coll]
  (letfn [(find-next-inc-seq [coll]
            (loop [coll coll inc-seq [-1]]
			  (if (or (= (first coll) nil) (<= (first coll) (last inc-seq)))
			      (rest inc-seq)
				  (recur (rest coll) (conj inc-seq (first coll))))))]
    (loop [coll coll longest (seq '())]
      (cond
	    (seq coll)
		  (let [next-inc-seq (find-next-inc-seq coll)]
	        (recur (subvec coll (count next-inc-seq))
		           (max-key count next-inc-seq longest)))
		(> (count longest) 1) longest
		:else []))))
;;   Eeeeurgh. That's all kinds of ugly. The algorithm, well, okay, the
;; algorithm? Works fine, but it's not very clojure-like. It has a double-nested
;; loop/recur in it and, well, frankly there's probably a better way to do this.
;; It's an iterative algorithm more or less crammed into a functional language,
;; and that's not cool.
;;   I just don't see what that better way is. Maybe I'm stuck thinking too
;; iteratively? Off to the Google, to see what solutions the Clojure-Wizards of
;; the Net have come up with!
		
;; From https://gist.github.com/hyone/1032985 by hyone: Holy hell.
(fn longest-inc-seq [coll]
  (reduce #(let [len-a (count %1)
                 len-b (count %2)]
             (if (and (> len-b 1) (> len-b len-a)) %2 %1))
          []
          (reductions
            (fn [xs y]
              (if (> y (last xs)) (conj xs y) [y]))
            [(first coll)]
            (rest coll))))
;; So, let's break this down.
;;   The main thing we need to look at here is the reductions. What reductions
;; does is basically runs a reduce, but instead of gathering all the values into
;; one at the end, it returns a sequence of the intermediate steps. So, what
;; this reduction is doing is accumulating increasing subseqs by conj-ing any
;; higher next inputs into a vector as it passes along the array. If this were a
;; reduce, you would end up discarding all the earlier increasing subseqs, and
;; returning only the very last one, but because it's reductions, it actually
;; places the increasing subseqs into a sequence! So, take a look at what
;; happens when you run this:
(reductions
  (fn [xs y]
    (if (> y (last xs)) (conj xs y) [y]))
  [(first [1 0 1 2 3 0 4 5])]
  (rest [1 0 1 2 3 0 4 5]))
;;   The results are ([1] [0] [0 1] [0 1 2] [0 1 2 3] [0] [0 4] [0 4 5]). You get
;; a sequence of each increasing subseq, then it resets when it hits a lower
;; one.
;;   From here, the reduce function simply picks the higher of the two and
;; returns it.
;; In fact, hold on. That reduce - we can use max-key like we did up top:
(fn longest-inc-seq [coll]
  (reduce #(let [longest (max-key count %2 %1)]
              (if (> (count longest) 1) longest []))
          []
          (reductions
            (fn [xs y]
              (if (> y (last xs)) (conj xs y) [y]))
            [(first coll)]
            (rest coll))))
;;   ...eh, gets us one line. I'm sure there's some way to collapse the let
;; followed by an if into a single form, but I can't think of it and if-let
;; doesn't apply.
;;   Still! The reductions solution shown here is an ingenious solution that I
;; would not have come up with on my own. What other magic do you have for me, O
;; Mighty Google?
  
;; http://yyhh.org/blog/2011/05/my-solutions-problems-no-51-75-4clojure-com
(fn [coll]
  (->> (partition 2 1 coll) 
    (partition-by #(- (second %) (first %))) 
    (filter #(= 1 (- (second (first %)) (ffirst %)))) 
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
;; what is this I don't even
  
;; 54 - Partition a Sequence
;; Original:
(fn cust-part [x coll]
  (loop [n x seq-list (list (take x coll)) coll (drop x coll)]
    (if (< (count coll) n)
      seq-list
	  (recur
  	    n
	    (concat seq-list (list (take n coll)))
	    (drop n coll)))))
;; Before we start, just a little formatting:
(fn cust-part [x coll]
  (loop [n x 
         seq-list (list (take x coll)) 
		 coll (drop x coll)]
    (if (< (count coll) n)
        seq-list
	    (recur n
	           (concat seq-list (list (take n coll)))
	           (drop n coll)))))
;;   So apparently, I had no knowledge of closures when I wrote this. Well, this
;; issue can be remedied!
(fn cust-part [n coll]
  (loop [seq-list (list (take n coll)) 
		 coll (drop n coll)]
    (if (< (count coll) n)
        seq-list
	    (recur (concat seq-list (list (take n coll)))
	           (drop n coll)))))
;; That looks much better.
;;   Okay, so, what's going on here? Well, what we've got is a loop/recur (seems
;; I was on a loop/recur spree!) in which, basically, you take n from the
;; collection until the collection is consumed, concat-ing the n things you take
;; to the sequence. There's a check in there to see if there's still enough to
;; take, and that's pretty much that.
;;   I have no actual, serious objections to this algorithm other than it uses
;; loop/recur and, frankly, there's probably a better way. So, let's think about
;; this.

;;   How about using reductions? Just learned about the function last problem,
;; how about applying it here?
(fn cust-part [n coll]
  (filter #(= (count %) n)
          (reductions #(if (< (count %1) n) (conj %1 %2) [%2])
                      [(first coll)]
                      (rest coll))))
;;   What this does is make reductions in the same manner as the solution to 53,
;; but then it filters them by elements equal to the count. Works fine, it's
;; pretty sweet and concise. It does a ton that it doesn't need to, but that
;; just adds to its malformed, lumpy charm. I like it. What other ways might we
;; go about this?
;;   How about take-while? Wait, no, that returns them one at a time. Hmm, maybe
;; take-nth? Oh, that's hilarious and almost completely unsuited. And it
;; wouldn't work. I could do split-at, or split-with, but that'd still be
;; enclosed in a loop/recur loop to detect the end.
;;   I could do a reduce, of course - but then, I use reduce too much as well.
;; (Something like "reduce with a function that will count the last member of
;; the sequence, if less than n, append, else start new sequence" would do the
;; trick, with special attention to the case of sequences not evenly divisible
;; by n). I won't though.
;;   What does The Great Google say about this problem? Enlighten me, oh great
;; Internets!
;;   I found a lot of loop/recur functions similar to my initial solution to the
;; problem, but then I found a couple of solutions like this:
;; https://github.com/reillywatson/4clojure/blob/master/54.clj
(fn part [n s]
  (when (>= (count s) n)
    (cons (take n s) (part n (drop n s)))))
;;   Very short, very sweet! The only issue I have with this solution is that it
;; does consume the stack. I mean, it's not an issue because this isn't going to
;; be munching on anybody's stack anytime soon, but, well, I prefer recur over
;; straight recursion.

;; 55 - Count Occurences
;; Original:
(fn cust-freq [coll]
  (reduce
    (fn [m n] (assoc m n (inc (get m n 0))))
	{}
	coll))
;;   So that's a pretty obvious reduction going on here; accumulate the counts
;; into a map, and, well, that's...pretty much it.
;; Hmm.
;;   Okay. See, this seems *so* blindingly obvious to me that it appears obvious
;; that I'm missing something, because this is more or less a trivial way to
;; solve it that uses boring old reduce. So. Tell me, Internets, what have more
;; creative people come up with?

;; From https://gist.github.com/chrisvest/952171, two similar solutions:
(fn [xs]
  (->> (group-by identity xs)
       (map (fn [[k v]] (vector k (count v))))
       (into {})))
	   
(fn [l]
    (let [m1 (#(group-by identity %) l)]
        (zipmap (keys m1) (map count (vals m1)))))
		
;;   Well, okay, that works too. Basically, group-by returns a map, with the key
;; as the result of the input function on the map and the values as the values
;; that match that key. So, by grouping by identity, it groups all instances of
;; the same value under a single key, allowing you to quite easily get the count
;; by simply calling (count) on the value.
;;   That's definitely less boring than my little old reduce, I'm not gonna lie.
;; I feel terribly plain now!