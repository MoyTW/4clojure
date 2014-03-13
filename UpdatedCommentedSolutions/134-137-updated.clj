;;;; 134 - A nil key
;;; Scratch: https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/134-wip.clj
;; Original:
(fn __ [k m]
  (= nil (get m k :not-present)))
;; This is basically trivial. There's a trap there if you try to do something like using an if to check, since it'll grab the nil as the value and the if won't fire, but it's not particularly difficult. You can also use contains?:
(fn __ [k m] (and (contains? m k) (= nil (m k))))
;; or do something with the keys themselves, or whatever. Not hard.

;;;; 135 - Infix calculator
;;; Scratch: https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/135-wip.clj
;; Original:
(fn __ [initial & args]
  (let [ops (partition 2 args)]
    (reduce #((first %2) %1 (second %2)) initial ops)))
;; This is another pretty easy one. The fact that you don't have to handle order of operations, grouping, or anything but a straight left-to-right traversal makes it effectively trivial. The solution above takes the first data member, then turns the rest of the sequence into operation-value pairs, then applies the pairs in turn.
;; I mean, the reduce is super ugly and janky, but that's not...really important. Actually, wait, it's bugging me.
(fn __ [initial & args]
  (let [ops (partition 2 args)]
    (reduce (fn [v [o n]] (o v n)) initial ops)))
;;   Not that my unhelpful single-character variable names are any better...
;;   I think I just made the code worse.
;; Whatever, what's another way we could do this without using partition into reduce? Well, actually it's pretty much what reduce is designed for - you have a bunch of things stored in a collection which combine to one value, in a order-specific manner. Hmm.
;; We could just use, like, a loop/recur but that would be silly. Let's not be silly, if we can avoid it. I mean, you can if that's your thing, and sometimes I'm very silly, but - hmm.
;; Thing is, this problem is strictly sequential. There aren't a lot of approaches you can take because of that. Ah, well, I'll leave it here.

;;;; 137 - Digits and bases
;;; Scratch: https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/137-wip.clj
;; Original:
(fn __ [n base]
  (let [v (mod n base)
        r (int (/ n base))]
    (if (zero? r)
        [v]
        (conj (__ r base) v))))
;; So, the very first idea here was to just use Java's in-built conversion abilities. Unfortunately, that doesn't work for two reasons. First off, 15 isn't 15, 15 is f, and second, there's a maximum radix. So, you can't really do it that way. Pity, huh?
;; So, I did a recursive solution, which proceeds by dividing repeatedly by the targeted base. The first improvement that jumps out is (int (/ n base)) could be (quot n base). Second, there's no reason it should eat the stack.
(fn __ [n base]
  (if (zero? n)
      [0]
      (loop [out '() n n]
        (if (zero? n)
            out
            (recur (conj out (mod n base)) (quot n base))))))
;; I don't really think there's a better way to convert than "Divide and mod repeatedly" - that was actually how I learned to do it in, what was it? middle school? high school? whenever it was I learned to convert from bases. Probably high school.