; Okay, first thing to note is that if n is greater than (count input-set) you just return empty.
; So, how do you generate k-combinations...?
; Let's look at
; 3 #{0 1 2 3 4}

; Well, hmm. Okay. By hand:
; (2 #{0} #{1 2 3 4}) (2 #{1} #{0 2 3 4}) (2 #{2} #{0 1 3 4}) (2 #{3} #{0 1 2 4}) (2 #{4} #{0 1 2 3})

; ...too big, hold on, let's do 2 #{0 1 2}
; (1 #{0} #{1 2}) (1 #{1} {0 2}) (1 #{2} #{0 1})
; #{0 1} #{0 2}   #{1 0} #{1 2}   #{2 0} {2 1}
; We compute duplicates here, though. That's not ideal.
; Well, first, let's rubber-duck. Okay, so, what I'm doing here is basically reducing it down to the special case of (1 #{some set}), which becomes a set of one-element sets. So, what we do is go back upwards, combining these, and we'll end up with the k-combinations.

; I don't like the how inefficient it is.

; How about, /w 3 {0 1 2 3 4}:
; Take first 2  [0 1] 2 3 4
; Take next     [0 2] 3 4
; Take next     [0 3] 4
; Run /w (rest set)
; Take first 2  [1 2] 3 4
; Take next     [1 3] 4
; Run /w (rest set)
; Take first 2  [2 3] 4
; Run /w (rest set)
; Reached end, return nil

; For 4 {0 1 2 3 4}:
; Take first 3  [0 1 2] 3 4
; oh dear that won't work the way I thought it would
; humph

; Okay, note that the take can only go up to (count) - n (for 3 of 5, take goes up to 2, for 4 of 5, goes to 1 - if 5 of 5, stays at 0).
; hmm
; Okay hold on what is 2 {0 1 2 3}?
; [0 1] [0 2] [0 3] [1 2] [1 3] [2 3]
; Goes up to 2, or 4-2
; and 2 {1 2 3 4} is
; [1 2] [1 3] [1 4] [2 3] [2 4] [3 4]
; ...what's 3 [1 2 3 4]?
; [1 2 3] [1 2 4] [1 3 4]
; What's 3 [2 3 4]?
; [2 3 4]

; I feel super slow. Like, I should not be struggling here, dangit, I learned stuff like this!
; 3 {0 1 2 3 4}, len=5
; First     {0 1 2} {[0]                                             [1]                     [2]}
; Second    {1 2 3} {[0 1]                   [0 2]           [0 3]   [1 2]           [1 3]   [2 3]}
; Third     {2 3 4} {[0 1 2] [0 1 3] [0 1 4] [0 2 3] [0 2 4] [0 3 4] [1 2 3] [1 2 4] [1 3 4] [2 3 4]}

; 4 {0 1 2 3 4}, len=5
; First value is    {0 1} [0] [1]
; Second value is   {1 2} [0 1] [0 2] [1 2] ; also [1 1]
; Third value is    {2 3} [0 1 2] [0 1 3] [0 2 3] [1 2 3]
; Fourth value is   {3 4} [0 1 2 3] [0 1 2 4] [0 1 3 4] [0 2 3 4] [1 2 3 4]

; Let's do the absolute dumbest thing - generate all of the sets. ALL of them.
(
(defn gen-next [p-sets n-set]
  (into #{} (for [p p-sets n n-set
                  :when (not (contains? p n))]
              (conj p n)))) 
#{#{0} #{1} #{2} #{3} #{4}} #{0 1 2 3 4})
; ...hmm.
(
(fn k-comb [n st]
  (let [st-of-sts (map (fn [n] #{n}) st)]
    (loop [i 1 out-set st-of-sts]
      (if (= i n) out-set
        (recur (inc i) (gen-next out-set st))))))
3 #{0 1 2 3 4})
; Looks like it works, actually.
(
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
1 #{1 4 5 6})

; so turns out I don't need all this down here

; Okay, we'll do it inefficient; we can clean it up on the revision pass. Just generate ALL sets, and grab the ascending ones.
; This means we're always going to be working with numbers, which is less than ideal - but we can map them to actual elements later along the line.
; So...all ascending sets.
(
(fn get-ascending [prev-sets new-set]
  (into #{} (for [x prev-sets y new-set
                  :let [st (conj x y)]
                  :when (apply < st)]
              st))) #{[0] [1] [2]} #{1 2 3})

; To generate the ranges:
(fn gen-ranges [