;; Hey, base conversions, there's probably totally a Java thing for that.
(.toString 9)
(.toString 9 2)
;; is it not an int wait my syntax is wrong isn't it
(Integer/toString 9 2)
;; huzzah that makes this very easy
(defn __ [n base]
  (map #(Integer. (str %)) (Integer/toString n base)))
  
(let [n (rand-int 100000)](__ n n))
;; hmm wait, so, it's in its own base? uuuuh

(__ 20 20)
(__ 99991 99991) ; is there a limit on the base or something? Time to read the java documentation.
;; Oh: "If the radix is smaller than Character.MIN_RADIX or larger than Character.MAX_RADIX, then the radix 10 is used instead." SWELL. I wonder if BigInt will be nicer?
;; NOPE.
;; Well that's annoying. Do we have to do it by hand, as it were, with divisions and mods and all that good stuff? Guess so.

(defn __ [n base]
  (let [v (mod n base)
        r (int (/ n base))]
    (if (zero? r)
        [v]
        (conj (__ r base) v))))
(__ 113 2)
(let [n (rand-int 100000)](__ n n))
;; We could clean it up (especially by putting it into loop/recur so it's not eating the stack) but hey, it works. We'll do that on the second pass, I guess.