;; No naive algs here, it warns the visitor.

;; Most obvious solution? Count up and divide?
;; Second most obvious solution: multiply them

;; 3, 5
;;  1  2  3  4  5  6
;;  3  6  9 12 15 18
;;  5 10 15 20 25 30
;; So 15, 30 are dups. So, if input % second = 0, do not mulitply first.

(defn __ [n a b]
  (loop [i 1 o 0]
    (if (and (> (* i a) n) (> (* i a) n))
        o
        (let [smaller (if (and (< (* i a) n)
                               (not= 0 (mod i b)))
                          (* i a) 
                          0)
              larger (if (< (* i b) n) (* i b) 0)]
          (recur (inc i) (+' o smaller larger))))))
(__ 10 3 5)
(__ 20 3 5)
(__ 100 3 5)
(= 78 (__ 20 3 5))
(= 23 (__ 10 3 5))
(= 233168 (__ 1000 3 5))
(__ 3 17 11)

;; ugly but does it work?
;; Works my in repl, but not in 4Clojure? What's going wrong here.
;; works on tryclj, but not - so that's usually something to do with sequences not being evaluated, isn't it?
;; Or is it actually a speed thing? (hard to imagine that it times out on the *first* one but maybe it doesn't strictly run them in order?)

(time (= "4530161696788274281" (str (__ (* 10000 10000 1000) 1597 3571))))
;; whoa elapsed 37564 msec that's definitely a timeout!
;; Okay, so, hmmmmmm. Is there an underlying math trick here or can you streamline the algorithm itself to be Not Taking Forever on the admittedly huge inputs?
;; No, there's a Math Trick here, I'm pretty sure. Something having to do with summing every number from 1 to n, which could then be multiplied?
;; 1*3 + 2*3 + 3*3 + 4*3 = (1+2+3+4) * 3 and you can do the divide by two summation thing to get the first term of the multiplication. And that's a linear time operation. So...yeah, we could do that.
;; Or we could try to speed this up to be acceptably fast.
;; Hmm.
;; You know what that's boring let's try to see if it's possible to speed this up to be acceptably fast.

;; Well, the thing about the alg I'm using is it can't take advantage of any of the concurrency that Clojure offers...
(Math/floor (/ (* 10000 10000 1000) 1597))
(Math/floor (/ (* 10000 10000 1000) 3571))

(Math/ceil (/ 27 3)) ;aaaand dec since we don't want it itself
(dec (Math/ceil (/ 27 3))) ; wait no range takes care of this

(defn resolve-a [n a b]
  (->> (range 1 (Math/ceil (/ n a)))
       (filter #(pos? (mod % b)))
       (map #(* % a))
       (reduce +)))
(defn resolve-b [n a b]
  (->> (range 1 (Math/ceil (/ n b)))
       (map #(* % b))
       (reduce +)))
(defn __ [n a b]
  (+' (resolve-a n a b) (resolve-b n a b)))
(__ 20 3 5)
(time (= "4530161696788274281" (str (__ (* 10000 10000 1000) 1597 3571))))
;; 31066 - definitely an improvement...
;; See, this is where I wish I knew more about concurrency in clojure, because what I would do normally is assign a bunch of threads (range 1 100) (range 100 200) ... (range ? end) and then sum them all at the end, but I don't quite know what's going on in Clojure's internals.
;; Okay, how about:
(def d (Math/floor (/ (* 10000 10000 1000) 1597)))
(def big (* 10000 10000 1000))

(defn f-a [n a b]
  (let [size-range (Math/floor (/ n a))
        one-r (range 1 size-range (int (/ size-range 3)))
        two-r (conj (vec (rest one-r)) size-range)]
    (->> (map vector one-r two-r)
         (map #(range (first %) (second %)))
         (map (fn [r] (filter #(pos? (mod % b)) r)))
         (map (fn [r] (map #(* % a) r)))
         (reduce #(+ %1 (reduce + %2)) 0)))) ; How to keep this from blowing heap
(f-a big 1597 3571)
(f-a 10 3 5)

;; Can we prevent by increasing # of maps (when runs reduce eval of sum is < heap size)
(defn f-a [n a b]
  (let [size-range (Math/floor (/ n a))
        one-r (range 1 size-range (int (/ size-range 1000))) ; smaller ranges
        two-r (conj (vec (rest one-r)) size-range)]
    (->> (map vector one-r two-r)
         (map #(range (first %) (second %)))
         (map (fn [r] (filter #(pos? (mod % b)) r)))
         (map (fn [r] (map #(* % a) r)))
         (reduce #(+ %1 (reduce + %2)) 0))))
(f-a big 1597 3571)
;; Yes that works

;; hhhhmmmmm okay do we actually see significant speedups here
(defn f-a [n a b s]
  (let [size-range (Math/floor (/ n a))
        one-r (range 1 size-range (int (/ size-range s)))
        two-r (conj (vec (rest one-r)) size-range)]
    (->> (pmap vector one-r two-r)
         (pmap #(range (first %) (second %)))
         (pmap (fn [r] (filter #(pos? (mod % b)) r)))
         (pmap (fn [r] (map #(* % a) r)))
         (pmap (fn [r] (reduce + r)))
         (reduce +))))
(time (f-a big 1597 3571 100))   ; 24795 msec
(time (f-a big 1597 3571 1000))  ; 11935 msec
(time (f-a big 1597 3571 10000)) ; 12382 msec
(time (resolve-a big 1597 3571)) ; 23657 msec
;; Okay I'm going to admit defeat and say that this probably won't get me the speedup I need. While it is significant...it's not nearly enough.
;; Maybe if I could use the GPU, but eh.

;; Okay, let's do what I was talking about earlier with the maths.

;; 1*3 + 2*3 + 3*3 + 4*3 = (1+2+3+4) * 3 and you can do the divide by two summation thing to get the first term of the multiplication. And that's a linear time operation. So...yeah, we could do that.

(defn sum-under [n]
  (/ (* n (inc n)) 2))

  (fn __ [n-i a-i b-i]
    (let [n (bigint n-i)
          a (bigint a-i)
          b (bigint b-i)
          sum-under (fn [n] (/ (* n (inc n)) 2))
          a-range (bigint (Math/floor (/ (dec n) a)))
          a-sum (bigint (* a (sum-under a-range)))
          b-range (bigint (Math/floor (/ (dec n) b)))
          b-sum (bigint (* b (sum-under b-range)))
          shared-range (bigint (Math/floor (/ (dec n) (* a b))))
          shared-sum (bigint (* (* a b) (sum-under shared-range)))]
      (bigint (- (+ a-sum b-sum) shared-sum))))
(__ 20 3 5)
(__ 3 17 11)

;; ugh that's dumb

(fn __ [n a b]
  (let [sum-under (fn [n] (/ (* n (inc n)) 2))
        sum-all (fn [t] (* t (sum-under (bigint (Math/floor (/ (- n 1) t))))))]
    (- (+ (sum-all a) (sum-all b)) (sum-all (* a b)))))