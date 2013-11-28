;;; 67-69 - In Which There Is A Bad Case Of Inelegance

;;; 67 - Prime Numbers
;; Original
(fn first-n-primes [n]
  (let [top-num (Math/ceil (+ (* n (Math/log n)) 
	                       (* n (Math/log (Math/log n)))))]
    (loop [coll (rest (rest (take (+ 5 top-num) (range))))
           primes []]
      (if (= (count primes) n)
          primes
          (let [no-div (filter #(> (mod % (first coll)) 0) coll)]
            (recur no-div (conj primes (first coll))))))))
;; The algorithm implemented here is the Sieve of Eratosthenes, which will find all primes up to the given maximum number n. Since we're searching for the first n primes, rather than all primes under n, we first calculate top-num to be the number under which there are n prime numbers (around n * log(n) +  n * log(log(n)) - this is an approximation for the nth prime). Then we use the Sieve, generating all primes up to that number.
;; As far as the specific implementation, hmm. Let me think. Is there a better way to do this? Well, the code to generate the initial sequence of numbers is kind of awkward (seriously, (rest (rest (take +5 top-num) (range)))? that's pretty darn clowny). We could clean up the "exclude things that are divisible" code. Minor improvements. Oh, and the formatting for the calculation of the nth prime is kind of messed up. So, let's go and do that:
(fn first-n-primes [n]
  (let [approx-nth (+ 5 (Math/ceil (+ (* n (Math/log n)) 
                                      (* n (Math/log (Math/log n))))))]
    (loop [coll (drop 2 (take approx-nth (range)))
           primes []]
      (if (= (count primes) n) primes
          (let [no-div (filter #(not= 0 (mod % (first coll))) coll)]
            (recur no-div (conj primes (first coll))))))))
;; I'm not sure it's worth the effort. Heck, I'm not even sure if it's better! Oh, well, that's how it goes sometimes.

;;; 68 - Recurring Theme
;; Original:
[7, 6, 5, 4, 3]
;; Wait, wait, wait. Hold on. We introduced Reduce with 64, and apparently loop/recur with 68 - so...were all the ones before this supposed to be done with old-fashioned recursion? Or, like, what?

;;; 69 - Merge with a Function
;; Original:
(fn cust-merge-with [f m & args]
  (reduce
    (fn merge-map [m t]
      (let [pairs (seq t)]
        (reduce
          (fn m-single [m t]
            (let [v-in-m (get m (first t))]
              (if (= nil v-in-m)
                (conj m t)
                (conj m {(first t) (f v-in-m (second t))}))))
          m
          t)))
    m
    args))
;; I have no idea what this is doing. Obviously it works, but at first glance, my gaze just slides off it. Let's dig in.
;; So, it runs a reduce with the function merge-map over the args. Okay. Got it - for each map, merge the map into the existing map(s).
;; merge-map runs its own reduce, and I think that the formatting is screwing with my head. Hold on, let's make this...less silly:
(fn cust-merge-with [f m & args]
  (reduce (fn merge-map [m t]
            (let [pairs (seq t)]
              (reduce (fn m-single [m t]
                        (let [v-in-m (get m (first t))]
                          (if (= nil v-in-m)
                              (conj m t)
                              (conj m {(first t) (f v-in-m (second t))}))))
                      m
                      t)))
          m
          args))
;; Okay I think I'm getting a better idea of what's going on, but why are these functions defined inside of the reduce like that? And m stands for map, but why is the next element named t? Does t stand for tuple or something? And I've got a let followed by an if nil using the same value, which we can compress into an if-let. Let's continue cleaning it up:
(fn cust-merge-with [f m & args]
  (letfn [(merge-single [m [k v]]
            (if-let [v-in-m (get m k)]
              (conj m {k (f v-in-m v)})
              (conj m [k v])))
          (merge-map [m new-m]
            (let [pairs (seq new-m)]
              (reduce merge-single m new-m)))]
    (reduce merge-map m args)))
;; So, now that it's somewhat less silly, it's a little easier to see what's going on. You combine two maps with merge-map, and merge-map calls on merge-single for every element of the new map. Merge-single will first check to see if the key is already in the map. If it is, it applies the function, otherwise it simply adds the key/value pair. So, actually, it's a lot saner then it at first looks - my formatting was just so beyond the pale it it appeared a raving work of insanity.
;; Surely a double-reduce isn't as elegant as it could be. Let's just take a peek at the source code of merge-with and...oh, hey, look at that.
;; The actual implementation of merge-with is surprisingly similar, and also uses a double-reduce. Huh!