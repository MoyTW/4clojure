((fn [v n]
  (loop [v v n n i 1]
    (if (= i n)
        v
      (let [n-c (- (* n i) i)]
        (recur (apply conj (subvec v 0 n-c) (subvec v n)) n (inc i)))))) 
		[1 2 3 4 5 6 7 8] 2)
	   
((fn [v n]
  (loop [v v, n n, i 0, e (int (/ (count v) n))]
    (if (= i e)
        v
      (let [n-c (- (* n (inc i)) i)]
	    (if (= (count v) n-c)
		    (subvec v 0 (dec n-c))
          (recur (apply conj (subvec v 0 (dec n-c)) (subvec v n-c)) 
		         n 
			     (inc i) 
			     e))))))
		[1 2 3 4 5 6 7 8 9] 3)

If it's positive, drop from front and add to end
If it's negative, take from end and add to front

[1 2 3 4 5]
Wait, we can use subseq? No. No we cannot.

Okay, well, we can use a very simple structure for this. Just a plain-old loop->recur.

; Testing out multi-sig functions.
((fn what
  ([] (println "None"))
  ([x]
    (do
	  (println "One")
	  (what)))) 0)

((fn rotate [r coll]
  (loop [r r coll (apply vector coll)]
    (if (= 0 r)
	    coll
	  (if (> r 0)
	    (recur (dec r) (conj (subvec coll 1) (first coll)))
		(recur (inc r) (conj (butlast coll) (last coll))))))) 2 [1 2 3])
		
((fn [coll n]
  (reduce 
    (fn [sub-seqs, next-v]
      (conj (subvec sub-seqs 1) (conj (first sub-seqs) next-v))) 
	(nth (iterate #(conj % []) []) n) 
	coll)) [1 2 3 4] 2)
		
(fn [coll n]
  (reduce 
    (fn [sub-seqs, next-v]
      (conj (subvec sub-seqs 1) (conj (first sub-seqs) next-v))) 
	(nth (iterate #(conj % []) [[]]) n) 
	coll))
		
(reduce (fn [sub-seqs, next-v]
          (conj (subvec sub-seqs 1)
		        (conj (first sub-seqs) next-v))) [[][]] [2 3 4 5])				
		
; Generates a sequence of n empty vectors
(nth (iterate #(conj % []) [[]]) %2))

; Multiplies every number in the range of 0, 2 by 5
(map #(* % 5) (range 0 2))		
		
((fn [b s]
  (let [d (/ b s)]
      (take d (iterate #(map inc %) ))
    )
  ) 10 5)

(conj [[1 2 3]] [4 5 6])
  
; Splits a sequence into two parts
(
(fn [x coll]
  (loop [n x in-coll coll out-coll []]
    (if (= n 0)
	    (conj [out-coll] in-coll)
      (recur (dec n) (subvec in-coll 1) (conj out-coll (first in-coll))))))
3 [1 2 3 4 5 6])

; #50 - split by type
; Create a mapping from types to vectors; for each data member, check type. If it's not
; a key, then create a new mapping to a vector. If it is, conj it to the existing
; vector.
;
; We can use reduce here (take a map)
(
(fn split [coll]
  (vals (reduce
    (fn [m n]
	  (assoc m (type n) (conj (get m (type n) []) n)))
	{}
	coll)))
[:a "foo" "bar" :b])

; #53 - longest increasing sub-sequence
(defn inc-sub-seq [coll, x]
  (loop [coll coll inc-seq [x]]
    (if (or (= (first coll) nil) (<= (first coll) (last inc-seq)))
        (rest inc-seq)
	  (recur (rest coll) (conj inc-seq (first coll))))))

(
(fn longest [coll]
  (loop [coll coll longest (seq '())]
    (if (= (first coll) nil)
	  (if (> (count longest) 1)
	    longest
		[])
	  (let [next-inc-seq (loop [coll coll inc-seq [-1]]
                           (if (or (= (first coll) nil) (<= (first coll) (last inc-seq)))
                               (rest inc-seq)
	                       (recur (rest coll) (conj inc-seq (first coll)))))]
	    (recur (subvec coll (count next-inc-seq))
		       (if (>= (count longest) (count next-inc-seq))
			     longest
				 next-inc-seq))))))
[1 0 1 2 3 0 4 5])

(
(defn inc-sub-seq [coll, x]
  (loop [coll coll inc-seq [x]]
    (if (or (= (first coll) nil) (<= (first coll) (last inc-seq)))
        (rest inc-seq)
	  (recur (rest coll) (conj inc-seq (first coll))))))
[1 2 2 3 4 5 0 1 1 1] -1)

; #54
(
(fn cust-part [x coll]
  (loop [n x seq-list (list (take x coll)) coll (drop x coll)]
    (if (< (count coll) n)
      seq-list
	  (recur
  	    n
	    (concat seq-list (list (take n coll)))
	    (drop n coll)))))
3 (range 9))

;# 55
(
(fn cust-freq [coll]
  (reduce
    (fn [m n]
	  (assoc m n (inc (get m n 0))))
	{}
	coll))
[1 1 2 3 2 1 1])

; #56
(
(fn cust-distinct [coll]
  (reduce
    (fn [m n]
	  (if (some #{n} m)
	    m
		(conj m n)))
	[]
	coll))
[1 2 1 3 1 2 4])

; #58
((
(fn run-in-order
  ([x] x)
  ([x y]
    (fn 
      ([arg] (x (y arg)))
  	  ([a b] (x (y a b)))
      ([a b c & args] (x (apply y a b c args)))))
  ([x y z]
	(fn 
	  ([arg] (x (y (z arg))))
	  ([a b] (x (y (z a b))))
	  ([a b c & args] (x (y (apply z a b c args)))))))
rest reverse) [1 2 3 4])

; #59
((
(fn cust-jux
  ([x] (fn [& args] (list (apply x args))))
  ([x y] (fn [& args] (list (apply x args) (apply y args))))
  ([x y z] (fn [& args] (list (apply x args) (apply y args) (apply z args)))))
+ max min) 2 3 5 1 6 4)

; #60
; Bad version of Reduce
(
(fn cust-reductions
  ([f coll] (cust-reductions f (first coll) (rest coll)))
  ([f init coll]
    (let [cseq (seq coll)]
      (if (= nil (first cseq))
	      init
	    (recur f (f init (first cseq)) (rest cseq))))))
+ [1 2 3 4])

; The actual thing we want - but not lazy! :(
(take 3 (
(fn cust-reductions
  ([f coll] (cust-reductions f (list (first coll)) (rest coll)))
  ([f init coll]
    (if (= nil (first coll))
	    (reverse init)
	  (recur f (cons (f (first init) (first coll)) init) (rest coll)))))
+ [1] [2 3 4]))

; Not tail-recursive version (that was easy)
(instance? clojure.lang.LazySeq (
(fn cust-reductions
  ([f coll] (cust-reductions f (list (first coll)) (rest coll)))
  ([f init coll]
    (if (= nil (first coll))
	    (reverse init)
	  (cust-reductions f (cons (f (first init) (first coll)) init) (rest coll)))))
+ [1] [2 3 4 5 6]))

; Semi-functional recursive!
(take 5 (
(fn cust-reductions
  ([f coll] (cust-reductions f (list (first coll)) (next coll)))
  ([f init coll]
    (cons 
	  init 
	  (lazy-seq 
	    (cust-reductions 
          f
	      (cons (f (first init) (first coll)) init) 
	      (next coll))))))
+ (range)))

; So, it is a lazy sequence. It just always times out when you run it.
; Well. Progress, I guess?
; holy crap I was forgetting to cons it to the front and it was never actually forming
; a sequence
(take 5 (
(fn cust-reductions
  ([f coll] (cust-reductions f (list (first coll)) (next coll)))
  ([f init coll]
    (cons 
	  init
	  (lazy-seq 
	    (cust-reductions 
          f
	      (cons (f (first init) (first coll)) init)
	      (next coll))))))
+ (range)))

; Copied Code
(take 5 (
(fn cust-reductions
  ([f coll] (cust-reductions f (first coll) (next coll)))
  ([f init coll]
    (if (seq coll)
      (cons init 
	    (lazy-seq 
  	      (cust-reductions 
            f
	        (f init (first coll))
	        (next coll))))
	  (cons init (lazy-seq '())))))
+ (range)))

; Copied Code
(take 5 (
(fn cust-reductions
  ([f coll] (cust-reductions f (first coll) (next coll)))
  ([f init coll]
      (cons init 
	    (lazy-seq 
  	      (cust-reductions 
            f
	        (f init (first coll))
	        (next coll))))))
+ 2 (range)))

(take 3 (
(fn ls [x] (cons x (lazy-seq (ls x)))) 
5 ))

; Returns a lazy sequence of the first n items (yes I know that's already there)
(take 3 (
(fn rev [c]
  (fn rev-seg [coll]
    (cons (first coll) (lazy-seq (rev-seg (drop 1 coll))))) c)
(range)))

; Tries to return a lazy sequence, but the rest call breaks the laziness
; wait
; no it doesn't! wtf?
(take 3 (
(fn rev [c]
  (fn rev-seg [coll]
    (cons (first coll) (lazy-seq (rev-seg (rest coll))))) c)
(range)))

; #61
(
(fn cust-zipmap [k v]
  (apply assoc {}(interleave k v)))
[:a :b :c] [1 2 3])

; #62
(take 100 (
(fn cust-iter [f x]
  (cons x (lazy-seq (cust-iter f (f x)))))
inc 0))

; #63
(
(fn cust-group [f s]
  (loop [f f s s m {}]
    (if (empty? s)
	    m
	  (let [f-s (first s)]
	    (recur 
	      f 
		  (rest s)
		  (assoc m (f f-s) (conj (get m (f f-s) []) f-s)))))))
#(> % 5) [1 3 6 8])

; #65
; To test if it's a set, add an already-existing member. If the size doesn't change, it must be a set!
; Maps will throw an IllegalArgumentException if you try to conj onto them
; Lists will conj onto the front
; Vectors will conj onto the back

;Therefore, our order of attack should be:
;	try: 
;	  (let [coll-c (conj coll (first coll))]
;	    (if (= (count (coll-c)) (count coll))
;			return :set
;        If conj'd to back, return vector, else return list
;	catch: return :map

; Works properly for everything but maps! You can't use try/catch in the REPL :(
(
(fn check-type [coll]
  (let [coll-c (conj coll (first coll))]
    (if (= (count coll-c) (count coll))
	  :set
	(if (= (last coll-c) (first coll))
	    :vector
	  :list))))
#{1 2 3 4})

; If you conj {:w :x :y :z}, the map will have its count up by two
(
(fn check-type [coll]
  (let [coll-c (conj coll (first coll))
        coll-m (conj coll {:w :x :y :z})]
	(if (= (+ 2 (count coll)) (count coll-m))
	    :map
      (if (= (count coll-c) (count coll))
	    :set
	    (if (= (last coll-c) (first coll))
	        :vector
	      :list)))))
{1 2 3 4})

(
(fn check-type [coll]
  (let [coll-m (conj coll {:w :x :y :z})]
	(if (= (+ 2 (count coll)) (count coll-m))
	    :map
      (let [coll-c (conj coll :z :z :x)]
        (if (= (+ 2 (count coll)) (count coll-c))
	      :set
	      (if (= (last coll-c) :x)
	          :vector
	        :list))))))
'(1 2 3 4 5 6))

; #66
(
(fn gcd [a b]
  (loop [a a b b]
    (if (= a b)
        a
      (if (> a b)
        (recur (- a b) b)
        (recur a (- b a))))))
1023 858)

; #67
; Gets the number of integers within which there are n primes
((fn get-rng [n] (Math/ceil (+ (* n (Math/log n)) (* n (Math/log (Math/log n)))))) 6)

; The sieve itself
(
(fn sieve[num-primes, top-num]
  (loop [coll (rest (rest (take (inc top-num) (range))))
         primes []
         end num-primes]
    (if (= (count primes) end)
        primes
      (let [no-div (filter #(> (mod % (first coll)) 0) coll)]
        (recur 
          no-div
          (conj primes (first coll))
          num-primes)))))
6 16)

; Finally
(
(fn first-n-primes [n]
  (let [top-num (Math/ceil (+ (* n (Math/log n)) (* n (Math/log (Math/log n)))))]
    (loop [coll (rest (rest (take (+ 5 top-num) (range))))
           primes []]
      (if (= (count primes) n)
          primes
        (let [no-div (filter #(> (mod % (first coll)) 0) coll)]
          (recur 
            no-div
            (conj primes (first coll))))))))
2)

; 69
; This handles one argument only.
(
(fn merge-map [f m t]
  (let [pairs (seq t)]
    (reduce
      (fn m-single [m t]
        (let [v-in-m (get m (first t))]
          (if (= nil v-in-m)
            (conj m t)
          (conj m {(first t) (f v-in-m (second t))}))))
      m
      t)))
 * {:a 2, :b 3, :c 4} {:z 2, :b 2})

((fn cust-merge-with [f m & args] args) * {} {:a [4 5], :c [8 9]} {:b [7]})
(
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
* {:a 2, :b 3, :c 4} {:z 2, :b 2} {:c 2})

; 70
; REGEX whooo
; Does most of what we want - but caps change the sort order!
(sort (re-seq #"[A-z]+" "Next target: the amp station!"))
; Here we go.
(sort-by clojure.string/lower-case (re-seq #"[A-z]+" "Next target: the amp station!"))
; Aaaand, as a function. Easy as pie.
#(sort-by clojure.string/lower-case (re-seq #"[A-z]+" %))

; 73
; First, the board represented by
[[:x :e :o]
[:x :e :e]
[:x :e :o]]
; is basically
[[:x :e :o] [:x :e :e] [:x :e :o]]

; so we can check very easily for horizontal wins!
(
(defn hori [coll n] (nth coll n))
[[:x :e :o][:x :e :e][:x :e :o]] 0)

; We can get the verticals by using the following function:
(
(defn vert [coll n] (reduce #(conj %1 (nth %2 n)) [] coll))
[[:x :e :o][:x :e :e][:x :e :o]] 0)

; We can check whether they're all the same with the following function:
(
(defn three-in-row [coll] 
  (let [result (reduce #(if (= %1 %2) %1 nil) (first coll) coll)]
    (if (or (= result :o) (= result :x))
      result
    nil)))
[:e :e :e])

(
(fn three-in-row [coll] 
  (let [result (reduce #(if (= %1 %2) %1 nil) (first coll) coll)]
    (if (or (= result :o) (= result :x))
      result
    nil)))
[:e :e :e])

; This checks the lines horizontally and vertically
(
(fn check-lines [coll]
  (loop [rng (take 3 (range))]
    (if (= '() rng)
        nil
      (let [n (first rng)
            v-val (three-in-row (hori coll n))
            h-val (three-in-row (vert coll n))]
        (if v-val
            v-val
          (if h-val
              h-val
            (recur (rest rng))))))))
[[:x :e :o] [:x :e :e] [:x :e :o]])

; Diagonals
(
(defn r-diag [coll]
  [(nth (nth coll 0) 0) (nth (nth coll 1) 1) (nth (nth coll 2) 2)])
[[:x :e :o] [:x :e :e] [:x :e :o]])
(
(defn l-diag [coll]
  [(nth (nth coll 0) 2) (nth (nth coll 1) 1) (nth (nth coll 2) 0)])
[[:x :e :o] [:x :e :e] [:x :e :o]])

; This should work?
; (it is so bad)
(
(fn check-lines [coll]
  (loop [rng (take 3 (range))]
    (let [three-in-row (fn three-in-row [coll]
                          (let [result (reduce #(if (= %1 %2) %1 nil) (first coll) coll)] (if (or (= result :o) (= result :x)) result nil)))]
    (if (= '() rng)
      (let [r-v (three-in-row [(nth (nth coll 0) 0) (nth (nth coll 1) 1) (nth (nth coll 2) 2)])
            l-v (three-in-row [(nth (nth coll 0) 2) (nth (nth coll 1) 1) (nth (nth coll 2) 0)])]
        (if r-v
            r-v
          (if l-v
              l-v
            nil)))
      (let [n (first rng)
            v-val (three-in-row (nth coll n))
            h-val (three-in-row (reduce #(conj %1 (nth %2 n)) [] coll))]
        (if v-val
            v-val
          (if h-val
              h-val
            (recur (rest rng)))))))))
[[:x :e :e] [:e :x :e] [:e :e :x]])

; #74
; strsplit
; The obvious method would be to generate a list of perfect squares, and then just filter by that list.
(
(fn get-squares [coll]
  (apply conj #{} (rest (take 
    (inc (Math/ceil (Math/sqrt (apply max coll)))) 
    (map #(* % %) (range))))))
[1 10 3 19 12])

; We can convert the string to nums as follows:
(
(fn to-nums [s]
  (apply conj #{} (map #(Integer. %) (clojure.string/split s #","))))
"15,16,25,36,37")

; aaalmost
(
(fn to-union [s]
  (let [nums (apply conj (sorted-set) (map #(Integer. %) (clojure.string/split s #",")))]
    (reduce #(str %1 "," %2)
      (clojure.set/intersection
        (apply conj (sorted-set) (rest (take 
          (inc (Math/ceil (Math/sqrt (apply max nums)))) 
          (map #(* % %) (range)))))
        nums))))
"15,16,25,36,37")

; Back to a string
(
(fn set-to-str [s]
  (reduce #(str %1 "," %2) s))
(sorted-set 16 25 36))

; 28 (again)
(
(fn c-flatten [coll]
  (let [l (first coll) r (next coll)]
    (concat 
      (if (sequential? l) (c-flatten l) [l])
      (if (sequential? r) (c-flatten r)))))
'((1 2) 3 [4 [5 6]]))

; 75
; We'll need our GCD code for this
(fn [a b] (loop [a a b b] (if (= a b) a (if (> a b) (recur (- a b) b) (recur a (- b a))))))

; Coprime if GCD=1; take (range) and filter by those with GCD=1!
(
(fn toitent [n]
  (let [gcd (fn [a b] (loop [a a b b] (if (= a b) a (if (> a b) (recur (- a b) b) (recur a (- b a))))))]
    (count (filter #(= (gcd % n) 1) (rest (take (inc n) (range)))))))
3)

; "Properly" formatted?
(fn toitent [n]
  (let [gcd (fn [a b] 
              (loop [a a b b] 
                (if (= a b) 
                  a 
                  (if (> a b) 
                    (recur (- a b) b) 
                    (recur a (- b a))))))]
    (count (filter #(= (gcd % n) 1) 
                   (rest (take (inc n) (range)))))))

; 77
; First, we should breaks a string down into a set
(fn str-to-set [s] (apply conj #{} s))

; We can use this set as a key to a map, which carries the set of words which break down into it.
(
(fn to-map [coll]
  (reduce 
    (fn [m n]
      (let [k (apply conj #{} n)]
        (assoc m k (conj (get m k #{}) n))))
    {}
    coll))
["meat" "mat" "team" "mate" "eat"])

; This is horrifyingly ugly
(
(fn anag [coll]
  (reduce #(conj %1 %2) #{}
  (filter
    #(> (count %) 1)
    (vals ((fn to-map [coll] (reduce (fn [m n] (let [k (apply conj #{} n)] (assoc m k (conj (get m k #{}) n)))) {} coll)) coll)))))
["veer" "lake" "item" "kale" "mite" "ever"])

; 78
(fn cust-tramp [f & args]
  (let [result (apply f args)]
    (loop [f result]
      (if (fn? f)
        (recur (f))
        f))))