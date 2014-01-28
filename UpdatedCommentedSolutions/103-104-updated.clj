;;;; 103 - Generating k-combinations
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/103-wip.clj
;; Original:
(fn k-comb [n st]
  (let [gen-next
         (fn [p-sets n-set]
           (into #{} (for [p p-sets n n-set
                           :when (not (contains? p n))]
                       (conj p n))))
        st-of-sts (map (fn [n] #{n}) st)]
    (loop [i 1 out-set st-of-sts]
      (if (= i n) (into #{} out-set)
        (recur (inc i) (gen-next out-set st))))))
;;   I remember having to do this in High School, but obviously whatever I
;; learned there didn't stick because I was pretty befuddled when I started to
;; formulate an algorithm for doing this. You can see me struggling if you go
;; and look at the scratch paper linked above.
;;   What's happening here is I'm basically repeatedly applying gen-next inside
;; of a loop/recur. What's gen-next do? It takes two sets, one being the set of
;; sets we want to build up and the other being the values we're combining into
;; the set of sets. It then mushes the values on the left into the values on the
;; right, but only if the ones in the right set don't already exist in the left
;; one, because that would create duplicates. Uh, hold on, this isn't a good way
;; of describing it.
;;   So, what gen-next does is basically takes the cross product of the two sets.
;; For example, we might have a set which we want to build up into all
;; k-combinations, looking like the following:
;;     #{#{1} #{2} #{3} #{4}}
;;   The set of values which we're using to build it would be:
;;     #{1 2 3 4}
;;   Now, if we weren't using sets, the cross product would look something like
;; [[1 1] [1 2] [1 3] [1 4] [2 2] ...] - but, since we're using sets, the cross
;; product will actually end up with one-element sets in the output set, due to
;; elements like [1 1]! So, we prevent those from being generated with the :when
;; clause - we only create the sets for which the added element isn't already in
;; the set.
;;   After one iteration, we'll have two-element sets in the output set. After
;; two iterations, we'll have three-element sets. So, we can do "Take k-sized
;; combinations" by just running it so many times.

;;   Now, obviously, there's some room for improvement here. Just in the writing
;; department, even! For example, my function takes two sets but I like
;; closures, who doesn't?
(fn k-comb [n st]
  (let [gen-next
         (fn [p-sets]
           (into #{} (for [p p-sets n st
                           :when (not (contains? p n))]
                       (conj p n))))
        st-of-sts (map (fn [n] #{n}) st)]
    (loop [i 1 out-set st-of-sts]
      (if (= i n) (into #{} out-set)
        (recur (inc i) (gen-next out-set))))))
;;   Also, that loop/recur is basically "Run this function n times on its own
;; output." So, we could use iterate here:
(fn k-comb [n elements]
  (let [gen-next
         (fn [sets]
           (into #{} (for [s sets e elements
                           :when (not (contains? s e))]
                       (conj s e))))]
    (into #{} (nth (iterate gen-next (map hash-set elements)) 
                   (dec n)))))
;;   That's undoubtably pretty darn ugly, near the end there. However, it's
;; definitely nicer than a loop/recur keeping an increment counter as it passes
;; over the data, at least conceptually (though not necessarily aesthetically!).
;; You'll notice that I haven't actually changed the algorithm, I've only
;; tinkered with the formatting.
;;   That's because actually changing the algorithm is, uh, difficult, and I
;; don't actually have a better idea of how to do it this time around than I did
;; the first. Does me little credit, I know, I know...

;;;; 104 - Write Roman Numerals
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/104-wip.clj
;; Original:
(fn write-roman [n]
  (let [split-digits
          (fn [n] (map #(Integer/parseInt (str %)) (str n)))
        digits (into [] (reverse (split-digits n)))
        process-place
          (fn [n one-sym five-sym ten-sym]
            (cond
              (= n nil) ""
              (<= n 3) (apply str (take n (repeat one-sym)))
              (= n 4) (str one-sym five-sym)
              (< n 9) (apply str five-sym (take (- n 5) (repeat one-sym)))
              (= n 9) (str one-sym ten-sym)))]
    (str (process-place (get digits 3) "M" "M" "M")
         (process-place (get digits 2) "C" "D" "M")
         (process-place (get digits 1) "X" "L" "C")
         (process-place (get digits 0) "I" "V" "X"))))
;;   So this is pretty interesting. I've basically made a huge branching if. What
;; I do is basically turn the integer into a string and break up all its digits
;; into their places values - ones, tens, hundreds, and thousands - and then
;; feed those into a mapping function which takes values and spits out Roman
;; numerals. Then, at the end, I put them all together, and magic!
;;   You know what I'm strangely attracted to this function. I think it's the
;; incredibly square str block, and the very very nice string block. There's
;; only one teensly little change I would make:
(fn write-roman [n]
  (let [split-digits (fn [n] (map #(Integer/parseInt (str %)) (str n)))
        digits (into [] (reverse (split-digits n)))
        process-place
          (fn [n one-sym five-sym ten-sym]
            (cond
              (= n nil) ""
              (<= n 3) (apply str (take n (repeat one-sym)))
              (= n 4) (str one-sym five-sym)
              (< n 9) (apply str five-sym (take (- n 5) (repeat one-sym)))
              (= n 9) (str one-sym ten-sym)))]
    (str (process-place (get digits 3) "M" "M" "M")
         (process-place (get digits 2) "C" "D" "M")
         (process-place (get digits 1) "X" "L" "C")
         (process-place (get digits 0) "I" "V" "X"))))
;;   Okay, but really. This approach is a more or less direct digit mapping from
;; Arabic numerals to Roman. And it's so very, very pretty, I think I'll just
;; leave it here for everybody to admire. ADMIRE IT. I think it is the prettiest
;; Clojure function I have ever written.