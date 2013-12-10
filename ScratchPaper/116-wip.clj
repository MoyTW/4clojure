;; ...so, the real issue here is finding the primes.
;; I'm tempted to just run me old sieve but that'd be kind of lame.
;; Oh, and I just Googled "next greatest prime" and apparently there's a BigInteger.nextProbablePrime that does what I want.

;; I am so very tempted to use that instead.

;; Screw it let's do it, I already know I can do basic primality tests, this is Java interop. Let's get this trainwreck on the rails!

(.nextProbablePrime (bigint 13))        ; breaks, no matching field found - probably it's not Java bigint
(type (bigint 13))                      ; clojure.lang.BigInt, we java.math.BigInteger
(type (biginteger 13))                  ; java.math.BigInteger - this is it
(.nextProbablePrime (biginteger 13))    ; 17 - sweet

;; So, we can use Java's BigInteger to get the next prime. How do we get the previous prime?

(.isProbablePrime (biginteger 13) 1000)     ; true
(.isProbablePrime (biginteger 18) 1000)     ; false
;; Scaling up the certainty (the 1000 in the previous call) reduces runtime.
;; This resolves to basically BigInteger(13).IsProbablePrime(1000)

(
(defn previousProbablePrime [n]
  (loop [n (dec n)]
    (if (.isProbablePrime (biginteger n) 500)
      n
      (recur (dec n)))))
22)

(
(fn is-balanced [n]
  (let [n-prime (.nextProbablePrime (biginteger n))
        p-prime (previousProbablePrime n)]
    (= n (/ (+ n-prime p-prime) 2))))
563)

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