;;;; 116 - Prime Sandwich
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/116-wip.clj
;; Original:
(fn is-balanced-prime [n]
  (let [previousProbablePrime
          (fn [n]
            (loop [n (dec n)]
              (if (.isProbablePrime (biginteger n) 500)
                n
                (recur (dec n)))))
        n-prime (.nextProbablePrime (biginteger n))
        p-prime (previousProbablePrime n)]
    (and (.isProbablePrime (biginteger n) 500)
         (= n (/ (+ n-prime p-prime) 2)))))
;;   Check http://en.wikipedia.org/wiki/Balanced_prime for a description of what
;; a balanced prime is. The problem is to write a function which detects if a
;; number is a balanced prime.
;;   The majority of the problem is actually in finding out whether a number is
;; prime, and then getting the previous and next primes. Helpfully, Java has a
;; very helpful BigInteger class which does all of the hard work for us! Well,
;; technically we're using biginteger, the Clojure function which casts its
;; argument to a BigInteger, but all the logic is handled through Java. The 500
;; that you see there is basically a confidence threshold, since
;; isProbablePrime() isn't actually assured to true iff the parameter's prime.
;; It's for speed and "good enough" reasons, I assume (primes are hard!).
;;   Anyways, being able to harness BigInteger to do all the hard lifting makes
;; the problem basically trivial, with the only work we have to do here being
;; finding the *previous* prime number. The solution that I came up with to find
;; the previous prime was to just start counting back from n until it hits a
;; prime number. Admittedly, the actual implementation is pretty janky and now
;; that I look at it, there's room for improvement. For one, that loop/recur is
;; fairly unnecessary:
(fn is-balanced-prime [n]
  (let [previousProbablePrime
          (fn [n]
            (if (.isProbablePrime (biginteger n) 500)
              n
              (recur (dec n))))
        n-prime (.nextProbablePrime (biginteger n))
        p-prime (previousProbablePrime (dec n))]
    (and (.isProbablePrime (biginteger n) 500)
         (= n (/ (+ n-prime p-prime) 2)))))
;;   Still, we can do better. How about something like:
(fn is-balanced-prime [n]
  (let [n-prime (.nextProbablePrime (biginteger n))
        p-prime (first (filter #(.isProbablePrime (biginteger %) 500) 
                               (iterate dec (dec n))))]
    (and (.isProbablePrime (biginteger n) 500)
         (= n (/ (+ n-prime p-prime) 2)))))
;;   We can reformat this a bit to be a bit more readable:
(fn is-balanced-prime [n]
  (if-let [p-prime (first (filter #(.isProbablePrime (biginteger %) 500) 
                          (iterate dec (dec n))))]
    (and (.isProbablePrime (biginteger n) 500)
         (-> n (biginteger) (.nextProbablePrime) (+ p-prime) (/ 2) (= n)))))
;;   Well, I'm not sure if that's actually more readable, but it's definitely
;; more compact! Also I finally used if-let, which has been rattling around in
;; the back of my head forever as one of those "nifty, but how many times will
;; you ever use it actually?" things. Also, I used ->! I'm so proud of myself.

;;;; 118 - Re-implement Map
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/118-wip.clj
;; Original:
(fn my-map [pred coll]
  (let [step (fn [p c]
                 (when-let [s (seq c)] ; when (seq c) is true, let s = (seq c), else return nil
                   (cons (pred (first s))
                         (my-map p (rest s)))))]
    (lazy-seq (step pred coll))))
;;   Okay, so, that seems a great deal more elegant than I usually write things.
;; Furthermore, the function body is indented to the beginning of the parameter
;; binding, which is totally not my style. Oh, and there's a comment in there!
;; So I suspected that I jacked this from somewhere when I looked at it and I
;; went and reviewed my scratch and hey, I did. I jacked it from the lazy-seq
;; documentation.
;;   It's pretty straightforward, though. The helper function basically advances
;; through the targeted sequence, applying the function to the sequence. The
;; main body just wraps the helper function, and presto! Easy map
;; implementation.
;;   It seems I got a little confused at some point, though, because aside from
;; the formatting I appear to be passing in the predicate as p, but that's
;; completely unnecessary, really:
(fn my-map [pred coll]
  (let [step (fn [c]
               (when-let [s (seq c)]
                 (cons (pred (first s))
                       (my-map pred (rest s)))))]
    (lazy-seq (step coll))))
;;   I wonder how other people did it?
;;   From 4clojure-golf
;; https://github.com/youz/4clojure-golf/blob/master/118_Re-implement%20Map.clj
;; we have a wonderfully obtuse solution:
;;; can't process empty sequences
(fn m [f [h & r]] (lazy-seq (cons (f h) (if r (m f r)))))
;;   It's certainly shorter than mine, and the fact that it doesn't contain a
;; subfunction makes it much less clunky than my own solution. As the function
;; says, though, it can't handle empty sequences - it inserts a nil where there
;; shouldn't be.
;;   There's a very similar solution from
;; https://github.com/qiuxiafei/4clojure/blob/master/answers/118.Re-implement%20Map
;; which is almost the exact same thing, except with a check specifically to
;; handle that case - but it doesn't return an empty sequence! Instead, it
;; returns nil when it drops out of the if:
(fn mymap [f coll]
  (if (false? (empty? coll))
        (lazy-seq
          (cons (f (first coll)) (mymap f (rest coll))))))
;;   Hmm. You could more accurately emulate map by putting in a return for an
;; empty sequence here. Oh, well - it's not like my sub-function based solution
;; is really more elegant, is it?
;;   It's not, by the way.