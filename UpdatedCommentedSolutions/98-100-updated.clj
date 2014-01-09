;;;; 98 - Equivalence Classes
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/98-wip.clj
;; Original:
(fn eq-classes [pred domain]
  (loop [classes #{} d domain]
    (let [class (filter #(= (pred %) (pred (first d))) d)
          next-classes (conj classes (into #{} class))
          rest-domain (apply disj d class)]
      (if (empty? rest-domain) next-classes
        (recur next-classes rest-domain)))))
;;   So the equivalence class with respect to a function is basically "All things
;; in the domain which resolve to the same reslt when the function is run on
;; them." So, the very first thought I had was, "Well, that's actually pretty
;; easy, because we can just filter the domain, right?" And it turned out it was
;; indeed surprisingly easy to do it that way.
;;   Let's walk through the algorithm:
;;     * Given a predicate and domain:
;;     * Take the first item in the domain, and run the function on it.
;;     * Run the function on everything else
;;     * Filter out of the domain all matches; this is your new class
;;     * Repeat
;;   Now, astute readers may well notice that, hey, this is basically group-by.
;; You run the function on everything, and when the result is equal, you group
;; them together. You could probably, like, do it in one line, couldn't you?
;;   Yes. Yes you can:
(fn eq-classes [pred domain]
  (set (map set (vals (group-by pred domain)))))
;;   Apparently, past-me did not consider group-by. Shame, past-me. Shame.

;;;; 99 - Produce Digits
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/99-wip.clj
;; Original:
(fn product-digits [x y]
  (map #(Integer/parseInt (str %)) (str (* x y))))
;;   This is pretty much trivial; you just multiply the two numbers, turn them
;; into strings, and turn them back. The same trick has been used numerous times
;; before in these 4Clojure exercises, so I'll just leave this here.

;;;; 100 - Least Common Multiple
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/100-wip.clj
;; Original:
(fn lcm-mult [& args]
  (letfn [(gcd [a b]
            (loop [a a b b]
              (let [[lrg sma] (reverse (sort [a b]))]
                (if (= lrg sma) lrg
                  (recur (- lrg sma) sma)))))
          (lcm [a b]
             (/ (* a b) (gcd a b)))]
    (reduce lcm args)))
;;   Quick! What's the Least Common Multiple mean? If you remember, you're better
;; at math than me, because I had to go look up the proper definition when I
;; first did this.
;;   The GCD code is from #66, copy-pasted. We won't be going over that (if you
;; want to you can go hunt down my updated version; it's in here somewhere, I
;; think). However, it does provide a very easy way to calculate the least
;; common mutiple (see
;; http://en.wikipedia.org/wiki/Least_common_multiple#Reduction_by_the_greatest_common_divisor):
;; LCM = (/ (* a b) (gcd a b)). So, that's actually pretty trivial, right? Note
;; that LCM(0,0) is special and explodes, so a proper function would, you know,
;; handle that. This one doesn't. Aside from that, though, the problem's
;; basically trivial!
;;   Except that we have to accept a variable number of arguments. Fortunately,
;; it's associative
;; (http://en.wikipedia.org/wiki/Least_common_multiple#Lattice-theoretic)
;; meaning that we can chain them very nicely! Since lcm(a, lcm(b, c)) = (lcm
;; (lcm(a, b), c)), we can freely just repeat the lcm.
;;   Funny how that works out, eh?
;;   Honestly? I don't think this one needs to be rewritten, as is. Of course if
;; this weren't 4Clojure, I'd use defn so we don't have that ugly letfn, but
;; hey, 4Clojure...