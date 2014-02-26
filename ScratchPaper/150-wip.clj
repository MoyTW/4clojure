;; This isn't really a numbers one is it? Wait it sorta is. hmm
;; it's an infinite sequence huh
;; "not less than" why not say gt-eq? w/e NI
;; Anyways, algorithm to generate next palindromic. Hmm.

(defn palindromic? [n]
  (let [take-half (fn [coll] (take (int (/ (count coll) 2)) coll))]
    (= (take-half (.toString n)) (take-half (reverse (.toString n))))))
(palindromic? "Zoo")
(palindromic? "ZoZ")

;; Okay, now. Hmm, what're all palindromics between 100 and 3000?
(sort (filter palindromic? (range 100 3000)))
;(101 111 121 131 141 151 161 171 181 191 202 212 222 232 242 252 262 272 282 292 303 313 323 333 343 353 363 373 383 393
; 404 414 424 434 444 454 464 474 484 494 505 515 525 535 545 555 565 575 585 595 606 616 626 636 646 656 666 676 686 696
; 707 717 727 737 747 757 767 777 787 797 808 818 828 838 848 858 868 878 888 898 909 919 929 939 949 959 969 979 989 999
; 1001 1111 1221 1331 1441 1551 1661 1771 1881 1991 2002 2112 2222 2332 2442 2552 2662 2772 2882 2992)

;; And how about some with more places...9900 and 15000?
(sort (filter palindromic? (range 9900 15000)))
;(9999 10001 10101 10201 10301 10401 10501 10601 10701 10801 10901 11011 11111 11211 11311 11411 11511 11611 11711 11811
; 11911 12021 12121 12221 12321 12421 12521 12621 12721 12821 12921 13031 13131 13231 13331 13431 13531 13631 13731 13831
; 13931 14041 14141 14241 14341 14441 14541 14641 14741 14841 14941)

;; So, a good algorithm would be "Go find the middle 1/2 characters, and increment."
;; So for 14841, you increment the 8 = +100 = 14941
;; For 9999, 9 -> = add...uh. Let's look at even-count ones:
(sort (filter palindromic? (range 5000 7000)))
; (5005 5115 5225 5335 5445 5555 5665 5775 5885 5995 6006 6116 6226 6336 6446 6556 6666 6776 6886 6996)
;; So. 1 -> 2 = +11. But 9999 -> 10001 = +2! You're not incrementing middle numbers, you're, wait, hold on.
;; So, "If the middle one is a 9, increment surrounding and set to 0".
;; hmmm, that's kinda a wave thing here

;; 9999
;; 99
;; 09 +1
;; 001
;; Mirror 001 (was even count so pivot not mirror) -> 10001

;; 13431
;; 134
;; 135
;; Mirror 135 (odd)-> 13531

;; 13931
;; 139
;; 140
;; Mirror 140 (odd) -> 14041

;; 99999
;; 999 + 1
;; 1000
;; Mirror 1000 (odd or even? - do pivot) -> 100001

(defn pivot [s]
  (concat s (rest (reverse s))))
(defn mirror [s]
  (concat s (reverse s)))

;; Okay, with odd:
(take (Math/ceil (/ (count (.toString 13931)) 2)) (.toString 13931))
(defn next-odd [n]
  (let [half (Integer/parseInt (apply str (take (Math/ceil (/ (count (.toString n)) 2)) (.toString n))))]
    (Integer/parseInt (apply str (pivot (.toString (inc half)))))))
;; wow uh that's some monkeying with parsing to strings and back there I'm doing
(= 13531 (next-odd 13431))
(= 14041 (next-odd 13931))
(= 100001 (next-odd 99999)) ;breaks
(next-odd 99999) ; 1000001 - extra 0!

(defn next-odd [n]
  (let [all-nines? (apply = \9 (.toString n))
        half (->> (.toString n)
                  (take (Math/ceil (/ (count (.toString n)) 2)))
                  (apply str)
                  (Integer/parseInt))
        next-half (.toString (inc half))]
    (if all-nines?
        (Integer/parseInt (apply str (mirror (butlast next-half))))
        (Integer/parseInt (apply str (pivot next-half))))))
(= 100001 (next-odd 99999)) ;yay

(defn to-int [coll]
  (Integer/parseInt (apply str coll)))

(defn next-even [n]
  (let [all-nines? (apply = \9 (.toString n))
        half (->> (.toString n)
                  (take (Math/ceil (/ (count (.toString n)) 2)))
                  (to-int))
        next-half (.toString (inc half))]
  (if all-nines?
    (to-int (pivot next-half))
    (to-int (mirror next-half)))))
(= 10001 (next-even 9999))
(= 1551 (next-even 1441))

;; We can put these together:
(defn pivot [s]
  (concat s (rest (reverse s))))
(defn mirror [s]
  (concat s (reverse s)))
  
(defn next-palindromic [n]
  (let [pivot (fn [s] (concat s (rest (reverse s))))
        mirror (fn [s] (concat s (reverse s)))
        is-even (even? (count (.toString n)))
        to-int (fn [coll] (Long/parseLong (apply str coll)))
        all-nines? (apply = \9 (.toString n))
        next-half (->> (.toString n)
                       (take (Math/ceil (/ (count (.toString n)) 2)))
                       (to-int)
                       (inc)
                       (.toString))]
    (if is-even
        (if all-nines?
            (to-int (pivot next-half))
            (to-int (mirror next-half)))
        (if all-nines?
            (to-int (mirror (butlast next-half)))
            (to-int (pivot next-half))))))
(next-palindromic 11)

(defn __ [n]
  (if (palindromic? n)
      (lazy-seq (cons n (__ (next-palindromic n))))
      (let [p (first (filter palindromic? (map  #(+ % n) (range))))]
        (lazy-seq (cons p (__ (next-palindromic p)))))))
(take 26 (__ 0))
(take 6 (__ 1234550000))

(defn tests []
  (and
    (= (take 26 (__ 0))
       [0 1 2 3 4 5 6 7 8 9 
        11 22 33 44 55 66 77 88 99 
        101 111 121 131 141 151 161])
    (= (take 16 (__ 162))
       [171 181 191 202 
        212 222 232 242 
        252 262 272 282 
        292 303 313 323])
    (= (take 6 (__ 1234550000))
       [1234554321 1234664321 1234774321 
        1234884321 1234994321 1235005321])
    (= (first (__ (* 111111111 111111111)))
       (* 111111111 111111111))
    (= (set (take 199 (__ 0)))
       (set (map #(first (__ %)) (range 0 10000))))
    (= true 
       (apply < (take 6666 (__ 9999999))))
    (= (nth (__ 0) 10101)
       9102019)))

(defn next-palindromic [n]
  (let [pivot (fn [s] (concat s (rest (reverse s))))
        mirror (fn [s] (concat s (reverse s)))
        is-even (even? (count (.toString n)))
        to-int (fn [coll] (Long/parseLong (apply str coll)))
        all-nines? (apply = \9 (.toString n))
        next-half (->> (.toString n)
                       (take (Math/ceil (/ (count (.toString n)) 2)))
                       (to-int)
                       (inc)
                       (.toString))]
    (if is-even
        (if all-nines?
            (to-int (pivot next-half))
            (to-int (mirror next-half)))
        (if all-nines?
            (to-int (mirror (butlast next-half)))
            (to-int (pivot next-half))))))
(next-palindromic 1234550000)
(next-palindromic 361)
(next-palindromic 163)

(defn closest-palindromic [n]
  (let [is-even (even? (count (.toString n)))
        take-half (fn [coll]
                    (->> coll
                         (take (Math/ceil (/ (count (.toString n)) 2)))
                         (to-int)))
        left-half (take-half (.toString n))
        right-half (take-half (reverse (.toString n)))]
     (if (> right-half left-half)
        (if is-even 
            (to-int (mirror (.toString (inc left-half))))
            (to-int (pivot (.toString (inc left-half)))))
        (if is-even
            (to-int (mirror (.toString left-half)))
            (to-int (pivot (.toString left-half)))))))

;; Okay, so apparently 4Clojure doesn't do string conversions/java interop/whatever well, so let's try rewriting without string conversions.
;; How would we mirror something just as a number?
;; 135 -> 13531
;; 135 * 10 + 3
;; (+ (* 10 %1) (mod 135 10))
;; Well, that's actually pivot, not mirror.
(defn mirror [n]
  (loop [out n in n]
    (if (zero? in)
        out
        (recur (+ (* 10 out) (mod in 10)) (int (/ in 10))))))
(mirror 135)
(defn pivot [n]
  (loop [out n in (int (/ n 10))]
    (if (zero? in)
        out
        (recur (+ (* 10 out) (mod in 10)) (int (/ in 10))))))
(pivot 135)

;; uuuuh, that's basically the same function
(defn dup [out in]
  (if (zero? in)
      out
      (recur (+ (* 10 out) (mod in 10)) (int (/ in 10)))))
(defn mirror [n]
  (dup n n))
(defn pivot [n]
  (dup n (int (/ n 10))))
(mirror 135)
(pivot 135)
;; Or we could have one function
(defn mirror [n parity]
  (let [begin (if (= parity :o) (bigint (/ n 10)) (bigint n))]
    (loop [out n in begin]
      (if (zero? in)
          out
          (recur (+' (*' 10 out) (mod in 10)) (bigint (/ in 10)))))))
(mirror 135 :o)
(mirror 135 :e)

(defn next-palindromic [n]
  (let [as-str (.toString n)
        parity (if (even? (count as-str)) :e :o)
        all-nines? (apply = \9 as-str)
        next-half (->> as-str
                       (take (Math/ceil (/ (count as-str) 2)))
                       (apply str)
                       (Long/parseLong)
                       (inc))]
    (if all-nines?
        (if (= parity :e)
            (mirror next-half :o)
            (mirror (int (/ next-half 10)) :e))
        (mirror next-half parity))))
(next-palindromic 19)
(next-palindromic 9)
(next-palindromic 99)
(next-palindromic 999)
(next-palindromic 9999)

(defn closest-palindromic [n]
  (let [as-str (.toString n)
        parity (if (even? (count as-str)) :e :o)
        take-half (fn [coll]
                    (->> coll
                         (take (Math/ceil (/ (count as-str) 2)))
                         (apply str)
                         (Long/parseLong)))
        left-half (take-half as-str)
        right-half (take-half (reverse as-str))]
    (if (> right-half left-half)
        (mirror (inc left-half) parity)
        (mirror left-half parity))))
(closest-palindromic 13)
(closest-palindromic 10)
(closest-palindromic 160)

(defn check-parity [n]
  (if (even? (int (Math/log10 n))) :o :e))
(check-parity 13522)
(check-parity 999)
(check-parity 12)

(defn take-half [n]
  (bigint (/ n (Math/pow 10 (bigint ((if (even? (int (Math/log10 n))) #(Math/floor %) #(Math/ceil %)) (/ (Math/log10 n) 2)))))))
(take-half 13522)
(take-half 3)
(take-half 20)
(defn take-half [n]
  (let [f (if (even? (int (Math/log10 n))) 
              #(Math/floor %) 
              #(Math/ceil %))]
    (bigint (/ n (Math/pow 10 (bigint (f (/ (Math/log10 n) 2))))))))

(defn next-palindromic [n]
  (let [parity (check-parity n)
        all-nines? (not= (int (Math/log10 n)) (int (Math/log10 (inc n))))
        next-half (inc (take-half n))]
    (if all-nines?
        (if (= parity :e)
            (mirror next-half :o)
            (mirror (int (/ next-half 10)) :e))
        (mirror next-half parity))))
(next-palindromic 135531)
(next-palindromic 13531)
(next-palindromic 9)
(next-palindromic 99)
(next-palindromic 999)
(next-palindromic 9999)

(defn reverse-integer [n]
  (loop [out 0 in n]
    (if (zero? in)
        out
        (recur (+' (*' 10 out) (mod in 10)) (bigint (/ in 10))))))

(defn closest-palindromic [n]
  (let [parity (check-parity n)
        left-half (take-half n)
        right-half (take-half (reverse-integer n))]
    (if (> right-half left-half)
        (mirror (inc left-half) parity)
        (mirror left-half parity))))
(closest-palindromic 13)
(closest-palindromic 10)
(closest-palindromic 160)