; 38 - 40, which I torture horribly because it's Halloween
; (that's a lie, I did it because I could)

; 38 - Maximum Value
; Original:
(fn custom-max [& args]
  (loop [current (first args) inputs (rest args)]
    (if (= (first inputs) nil) current
      (recur (if (> (first inputs) current) (first inputs) current )
             (rest inputs)))))
;   Ah, I've finally gotten to the point where past-me's formatting is
; decently readable and doesn't poke your eyes out with a snigger and a tad
; more malice than I'm totally comfortable with.
;   So, what we have here is a traditional "Iterate over everything in the
; data set and pick the one that meets a certain criteria (here, highest)."
; It basically consumes the set, compares the new value to the old value, and
; picks the highest, repeat until done. Nothing that we haven't seen before,
; yes?

;   And, like so many other "Consume a sequence" algorithms, it can be
; re-done with reduce like so:
(fn custom-max [& args]
  (reduce #(if (> %1 %2) %1 %2) in))
  
;   But as I've said, I'm overusing reduce. What would be another way to do
; this? Hum, hum.
;   A very naughty thought just occured to me. Filter. See, we can pick a
; thing, filter for things greater than it, and once there aren't any, it's
; the greatest! Here, hold my beer:
(fn why [& args]
  (loop [current (first args) inputs (rest args)]
    (let [gt (filter #(> % current) inputs)]
      (if (empty? gt) current
        (recur (first gt) (rest gt))))))
; It's genius! Genius, I tell you!
; (it is not genius)

; 39 - Interleave Two Seqs
; Original:
(fn customer-interleave [left right]
  (loop [left left right right new []]
    (if (or (= (first left) nil) (= (first right) nil) )
      new
      (recur (rest left)
             (rest right)
             (conj new (first left) (first right))))))
;   I have no idea why I named it customer-interleave. I suspect I was
; going for customer and had something else on the mind.
;   Anyways, what this fine function is doing is taking a left and right
; sequence and then consuming them in parallel, stopping when either of
; them bottoms out. It puts the consumed values into a new vector,
; helpfully titled new, and returns that.
;   As far as algorithms go, it's pretty darn solid, if I may say so
; myself. Fortunately, we actually can't just call reduce again (well, I
; mean, you can, and doubtless there's some way to make it work if you
; apply the thumbscrews enough, but you know what I mean, right?)
;   You know what, today seems to be thumbscrew night, what with my
; previous wacky terrible new solution. Let's do it!

;   Okay, here's what we're gonna do.
; We take the shorter one, right? We count that. That's how many things we
; want from both of them. Now, reduce only works on one sequence, right? So
; we've got to find a way to make it work on both. So, we'll use nth. We'll
; take n things from range, and then call nth on both of the sequences.
; That way we can use one sequence to work on two sequences.
;   4Clojure has them in vectors so we could theoretically map them in but
; this is supposed to work on sequences. Well. Lists and vectors.
; Sequential sequences. Anyways my point is no vector-specific operations!
(fn c-interleave [left right]
  (let [len (min (count left) (count right))
        indices (take len (range))]
    (reduce #(conj %1 (nth left %2) (nth right %2)) [] indices)))
; Yeah, that's pretty tortured.

; Alternative if you're sane you could totally just use map:
(fn c-interleave [left right]
  (mapcat #(vector %1 %2) left right))
  
; 40 - Interpose a Seq
; Original:
(fn custom-interpose [sep, in-seq]
  (butlast (flatten 
   (map (fn [in, sep] [in sep]) in-seq (take (count in-seq) (repeat sep))))))
;   Well, that's pushing 80 characters there on the line lengths but squeaks
; in juuust under. Still, that's uuuug-ly.
;   Also I apparently used map for this. Hmm. Looks like my past-self got
; in on the torturing early! So, what it's doing is generating a sequence
; of (count sequence)s of the to-be-interposed, then mapping the two
; sequences together. Then it hits them with the flatten hammer.
;    The issue here is what if you're got nested sequences? Then that
; flatten hammer will mess you up right quick. It's for this reason I think
; flatten should probably be avoided unless you absoulutely know exactly
; how deep your nesting will or won't be.
;   Anyway it ends up with the butlast because you'll have a trailing
; sep-val with this technique.
;    We could actually rewrite this to use mapcat without changing the
; algorithm, as so:
(fn c-interpose [sep in-seq]
  (butlast (mapcat #(vector %1 %2) 
                   in-seq 
                   (take (count in-seq) (repeat sep)))))
;   I'm still not sure on my formatting. Blocky-style better? Worse? Maybe
; I should have done a 2-space indent instead of indenting all the way to
; the end of mapcat.

;   Anyways I'm not super fond of this algorithm because we're generating
; an extra sequence, and we're mapping where we probably should be
; reducing, as so:
(fn c-interpose [sep in-seq]
  (butlast (reduce #(conj %1 %2 sep) [] in-seq)))
; Clocks in at a mere two lines. Excellent!