; Take Two, now with more comments!

; 30 - Compress a Sequence
; Original:
#((fn noconsec [in out]
    (if (empty? in)
      out
      (if (= (first in) (first (rest in)))
        (noconsec (rest in) out)
        (noconsec (rest in) (conj out (first in))))))
  % [])
;   So, what this is doing, is recursively consuming the input. If the head of
; the sequence to be consumed is equal to the one-past-the-head of the sequence
; to be consumed, it continues on consuming without adding it to the output,
; thereby skipping duplicates.
;   As far as algorithms go, it's fine. Gets the job done, and all that. No
; horrendous inefficiencies present themselves and it will do fine with a zero-length sequence, or a sequence with all one letter, so...
  
; ...let's go to cleanup. Algorithm is unchanged, but now it's prettier.
; I was on quite the #() binge, wasn't I? To loop/recur:
(fn noconsec [coll]
  (loop [in coll out []]
    (cond
      (empty? in) out
      (= (first in) (fnext in)) (recur (rest in) out)
      :else (recur (rest in) (conj out (first in))))))
;   Does basically the same thing, but no #-defined megafunction-wrapper-thing,
; and (first (rest in)) is replaced with (fnext in). Fnext. Fnext. Say that out
; loud, and see if you can avoid snickering.
;   Fnext.
;   My current personal rule on "if" statements is if it hits a nested if, pull
; it into a cond. That may change, though!

;    Anyways, we could do it with reduce really easily. Behold!
(fn noconsec [coll]
  (reduce #(if (= %2 (last %1)) %1 (conj %1 %2)) [] coll))
;   Unsure about whether the if statements in #-defined functions are something
; I should be using - they're a little long and possibly unclear - but then 
; again I use ternary operators in C++/C# without qualms, so...

; 31 - Pack a Sequence
; Original:
#((fn noconsec [in out]
    (if (empty? in)
      (reverse out)
      (if (= (first in) (first (first out)))
        (noconsec 
		  (rest in)
          (conj (rest out) (conj (first out) (first in))))
        (noconsec (rest in) (conj out (list(first in)))))))
  (rest %) (list (list (first %))))
;   My but that's ugly. Another recursive function designed to consume a
; sequence one element at a time, with the stop condition being an empty
; in-sequence. The difference here is if it finds a duplicate it packs it into
; a sub-list. In fact, it packs *every* element into a sub-list - if it's not a
; duplicate it gets its own 1-member list, making the output a list of lists.
;   So what it's doing is basically:
;   * If the incoming element is equal to the first value of the head sub-list
;       * Call self with cut-off head of output augmented with addition of 
; incoming element
;   * Otherwise, make a new sub-list and put that into output, call self
;   The logic's pretty straightforward, if hard to see in that swirling mass of
; conj-first-something statements.
  
;   First things first, just a little general cleaning up 'round the edges
(fn pack-seq [coll]
  (loop [in (rest coll) out (list (list (first coll)))]
    (cond
      (empty? in) (reverse out)
      (= (first in) (ffirst out)) 
        (recur (rest in) (conj (rest out) (conj (first out) (first in))))
      :else (recur (rest in) (conj out (list (first in)))))))
;   That huge recur statement's still looking ugly, though. Hmm. Okay, well,
; I cannot for the life of me think how to modify the outgoing collection 
; except by doing the awkward (conj (rest) (new head)) thing I've got going
; already. Which is...kind of weird, I'm sure there's some way of doing it, but
; it's slipping my mind. Well, let's see if using cond-let helps?
(fn pack-seq [coll]
  (loop [in (rest coll) out (list (list (first coll)))]
    (cond-let [fin (first in) rin (rest in)]
      (empty? in) (reverse out)
      (= (fin) (ffirst out)) 
        (recur (rin) (conj (rest out) (conj (first out) (fin))))
      :else (recur (rin) (conj out (list (fin)))))))
; Not really. A pity, that.

;   So, like most of these, you can use reduce to do it. I wonder if my current
; possible over-usage of reduce will be frowned upon by future-me?
(fn pack-seq [coll]
  (letfn [(p-reduce [out n] 
            (if (= n (ffirst out))
              (conj (rest out) (conj (first out) n))
              (conj out (list n))))]
    (reverse (reduce p-reduce [] coll))))
;   Shoved all the logic into a helper function. I get really bad with doing
; that, later on. Fair warning. Like, it gets downright awful.

; 32 - Duplicate a Sequence
(fn dup [in]
  (loop [in-seq in, out-seq []]
    (if (= (first in-seq) nil) 
      out-seq
      (recur (rest in-seq) (conj out-seq (first in-seq) (first in-seq) )))))
;   Algorithm here is "Consume incoming: if it's not empty, add two instances
; of the first member onto the outgoing." Very simple, very easy.

;   Again, cleaning up without touching the underlying algorithm. At least I 
; appear to have exited my "Define all of these wtih #!" phase.
;   Actually, it's pretty decent code. The recur is a little long but it's not
; over 80, and so...that (= (first in-seq) nil) bugs me, though.
(fn dup [in]
  (loop [in-seq in, out-seq []]
    (if (empty? in-seq) 
      out-seq
      (recur (rest in-seq) (conj out-seq (first in-seq) (first in-seq))))))
;   There. I'm considering replacing that double (first in-seq) with something
; like (take 2 (repeat (first in-seq))) but that really doesn't get us 
; anywhere, either in terms of length (smaller's better, usually) or 
; actual readability.

; Anyways, we can do a reduce here:
(fn dup-red [in]
  (reduce #(conj %1 %2 %2) [] in))

;   Or, heck, since I'm nattering on about me abusing reduce to an unhealthy
; degree, how about using map?
(fn dup-map [in]
  (mapcat #(vector % %) in))
;   Mapcat is "Do map, then do concat on the results." It's actually very
; similar to what reduce is supposed to do, just somewhat more limited.
  
; 33 - Replicate a Sequence
(fn dup [in, n]
  (loop [in-seq in, out-seq [], cnt n]
    (if (= (first in-seq) nil) 
      out-seq
      (recur (rest in-seq) 
             (loop [out-seq out-seq, element (first in-seq), cnt cnt]
               (if (= cnt 0) 
                 out-seq
                 (recur (conj out-seq element) element (dec cnt))))
             cnt))))
;   Oh dear, this is ugly, isn't it?
;   It's basically the same as 32, but instead of simply doubling it, you apply
; an arbitrary number of repetitions. The way I ended up hacking it was a recur
; in a recur - one recur to consume the incoming sequence, and then another
; recur that basically did "Run (conj (think)) on the output sequence n times,"
; using a decrementing counter.
;   Frankly it's pretty terrible. The algorithm, well, it's...okay? I mean,
; there are better tools for the job, but...well, if I were forced to do it
; with only basic recursion, I'd probably do it the same way again. It seems
; like it's trying a little too hard to be a nested for loop, though.

;   This was written before I knew about closures, so...let's tidy up a bit.
; Again, we're not changing the algorithm, just reformatting it a tad. Oh! And
; here's a good chance to use that (take n (repeat (blah))) thing I was 
; thinking about earlier!
(fn dup [in, n]
  (loop [in-seq in, out-seq []]
    (if (= (first in-seq) nil) 
      out-seq
      (recur (rest in-seq)
             (apply conj out-seq (->> in-seq (first) (repeat) (take n)))))))
             ;(apply conj out-seq (take n (repeat (first in-seq))))))))
             ; which of these is more readable, do you think?

; Of course, we can do it with reduce:
(fn dup-red [in n]
  (reduce #(apply conj %1 (take n (repeat %2))) [] in))

; Or, similarly, a map
(fn dup-map [in n]
  (mapcat #(take n (repeat %)) in))
; That whole thing can compress down to two-line statements! (One, if you don't
; mind putting the function name and parameters on the same line). I think I
; like Clojure.