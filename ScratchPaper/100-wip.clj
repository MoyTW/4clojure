; Oh, hey, high school math I should remember how to do.
; 'kay do I still have that euclid's gcd thing lying around from before?

; here it is:
(fn gcd [a b]
  (loop [a a b b]
    (if (= a b)
      a
      (if (> a b)
        (recur (- a b) b)
        (recur a (- b a))))))
; Well since my personal rule is nested ifs get conds, let's see if that cleans it up at all:
(fn gcd [a b]
  (loop [a a b b]
    (cond
      (= a b) a
      (> a b) (recur (- a b) b)
      (< a b) (recur a (- b a)))))
; long, how about
(defn gcd [a b]
  (loop [a a b b]
    (let [[lrg sma] (reverse (sort [a b]))]
      (if (= lrg sma) lrg
        (recur (- lrg sma) sma)))))
; eeeeh, well, guess it's a matter of taste, really...they all work

; Anyways. LCM(a, b) = (/ (abs (* a b)) gcd(a, b))
(fn lcm [a b]
  (/ (Math/abs (* a b)) (gcd a b)))
  
; Becomes
(defn lcm [a b]
  (letfn [(gcd [a b]
            (loop [a a b b]
              (let [[lrg sma] (reverse (sort [a b]))]
                (if (= lrg sma) lrg
                  (recur (- lrg sma) sma)))))]
    (/ (Math/abs (* a b)) (gcd a b))))
    
; So, how do we do an arbitrary number of numbers?
; Associative law: lcm(a, lcm(b, c)) = (lcm (lcm(a, b), c))
; so just run it on all the inputs
(defn lcm-mult [& args]
  (letfn [(gcd [a b]
            (loop [a a b b]
              (let [[lrg sma] (reverse (sort [a b]))]
                (if (= lrg sma) lrg
                  (recur (- lrg sma) sma)))))
          (lcm [a b]
             (/ (* a b) (gcd a b)))]
    (reduce lcm args)))
; Removed the Math/abs, the ratios threw it off and problem specifies positive only.