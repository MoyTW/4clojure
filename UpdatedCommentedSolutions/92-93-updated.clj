;;;; 92 - Read Roman numerals
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/92-wip.clj
;; Original:
(fn to-arabic [s]
  (let [numerals {"" 0 "I" 1, "IV" 4, "IX", 9, "V" 5, "X" 10, "XL" 40, "XC" 90,
        "L" 50, "C" 100, "CD" 400, "CM" 900, "D" 500, "M" 1000}
        p-n (fn [sum-hold n]
              (let [i-sum (first sum-hold)
                    i-hold (last sum-hold)
                    n-str (str i-hold n)]
                (condp contains? n-str
                  #{"I" "X" "C"} [i-sum n-str]
                  #{"II" "XX" "CC"} [(+ i-sum (get numerals (str i-hold))) n]
                  #{"IV" "IX" "XL" "XC" "CD" "CM"}
                    [(+ i-sum (get numerals n-str)) nil]
                  [(+ i-sum (get numerals (str i-hold))) n])))]
    (let [pair (reduce p-n [0 nil] s)]
      (+ (first pair) (get numerals (str (last pair)))))))
;;   Roman numerals are kind of funny; if you put a single smaller numeral after
;; the next step up, it subtracts. So, what you need to be able to do in order
;; to parse Roman numerals is to hold the previous character while parsing the
;; next - and if the previous character is lower than the next, that's how you
;; know it's a 4 instead of a five.
;;   So, that's what this algorithm does. It reduces over the string, holding the
;; last character in i-hold, which can be nil. When it encounters a new
;; character, it appends to i-hold the new character (and if i-hold is nil, the
;; resulting string is the new character alone). Then, it checks to see what the
;; resulting string is:
;;   * Is a possible subtractor ("I", "X", "C") - store in i-hold, to make sure
;; if the next character turns it into a subtractor it'll parse properly
;;   * Is a doubled possible subtractor - add, store new in i-hold; we can add
;; because it's impossible for it to be a subtractor
;;   * Is a confirmed subtrator - clear i-hold and add the value; it's a
;; definite subtractor, which means we don't look backwards from this point
;;   * Otherwise: Add the value of i-hold, new character is new i-hold
;;   Okay, I'll admit, that's a little confusing when I type it out like that:
;; there's probably a much more clear and concise way to explain the algorithm!
;; Oh well. Let's get to the code:

;;   First, we have the let, which is a map from character sequences to values.
;; We use this as a lookup table, once we parse the numerals into singles or
;; pairs.
(let [numerals {"" 0 "I" 1, "IV" 4, "IX", 9, "V" 5, "X" 10, "XL" 40, "XC" 90,
      "L" 50, "C" 100, "CD" 400, "CM" 900, "D" 500, "M" 1000}

;;   The majority of the logic occurs in the reduce function, p-n (short for
;; process-numeral) which accumulates into an array of [sum hold]. The original
;; code is...inelegant; there's a let [x (first v) y (last v)]! The condp clause
;; is...confusingly formatted. Let's see what we can do:
(fn [[sum hold] n]
  (let [numeral (str hold n)]
    (condp contains? numeral
      #{"I" "X" "C"} 
        [sum numeral]
      
      #{"II" "XX" "CC"} 
        [(+ sum (get numerals (str hold))) n]
      
      #{"IV" "IX" "XL" "XC" "CD" "CM"}
        [(+ sum (get numerals numeral)) nil]
      
      [(+ sum (get numerals (str hold))) n])))
;;   That's clearer, but terribly unfun to look at. We could also try indentation
;; with four characters (two kind of gets lost in the noise), like so:
(fn [[sum hold] n]
  (let [numeral (str hold n)]
    (condp contains? numeral
      #{"I" "X" "C"} 
          [sum numeral]
      #{"II" "XX" "CC"} 
          [(+ sum (get numerals (str hold))) n]
      #{"IV" "IX" "XL" "XC" "CD" "CM"}
          [(+ sum (get numerals numeral)) nil]
      [(+ sum (get numerals (str hold))) n])))
;;   I'm not really on board with any of the formatting options here, really.
;; Well, I guess we'll go with indentation. We tie it together with the
;; following two lines:
...
(let [pair (reduce p-n [0 nil] s)]
  (+ (first pair) (get numerals (str (last pair)))))))
;;   We need to actually go ahead and add on the last iteration because the
;; reduce won't do that for us if it ends in, say, "II" - it'll be one short. If
;; the reduce does consume the whole sequence (ends in, say, "IV" or something)
;; then the lookup will go to ""=0, and this step wouldn't have been necessary.
;;   You could do something like an if if you wanted to be more explicit but this
;; ends up working just as well...though, potentially it's more confusing.
;;   Note that none of the rewriting changed the algorithm, because, well, I
;; couldn't really think of a better way to do it! It's inelegant but you're
;; going to have to hold onto the...hmm. Actually, wait. Hmm.
;;   You could do something like "Find all the IV, IX, XLs, count them, count all
;; Is remaining"...instead of doing it line-by-line, you could basically
;; decompose it into tokens and, uh, hmm, that's kinda what I'm doing already
;; but here, let me show you what I mean:
(fn to-arabic [s]
  (let [numerals {"I" 1, "IV" 4, "IX", 9, "V" 5, "X" 10, "XL" 40, "XC" 90, "L" 50, "C" 100, "CD" 400, "CM" 900, "D" 500, "M" 1000}
        p-n (fn [[sum num-str] num-token]
              (let [matches (re-seq (re-pattern num-token) num-str)
                    cleaned-str (clojure.string/join (clojure.string/split num-str (re-pattern num-token)))]
                [(+ sum (* (count matches) (get numerals num-token))) cleaned-str]))]
    (let [[sum _] (reduce p-n [0 s] (sort-by count > (keys numerals)))]
      sum)))
;;   So, basically, this reduces over the keys of the map, and each time it
;; reduces it removes the key from the string and adds the appropriate sum. So,
;; what we do for the string "MMMCMXCIX" would be:
;;   * Count instances of "CD" = 0, proceed with [0 "MMMCMXCIX"]
;;   * Count instances of "CM" = 1, proceed with [900 "MMMXCIX"]
;;   * and so on and so on until we've exhausted the numerals
;;   Admittedly, the code's a bit difficult to read in its current form. Also?
;; I'm not sure what is up with those nested let statements. Can we clean it up
;; a little?
(fn to-arabic [s]
  (let [numerals {"I" 1, "IV" 4, "IX", 9, "V" 5, "X" 10, "XL" 40, "XC" 90,
                  "L" 50, "C" 100, "CD" 400, "CM" 900, "D" 500, "M" 1000}
        p-n (fn [[sum num-str] num-token]
              (let [num-matches (count (re-seq (re-pattern num-token) num-str))
                    cleaned-str (clojure.string/join (clojure.string/split num-str (re-pattern num-token)))]
                [(+ sum (* num-matches (get numerals num-token))) cleaned-str]))]
    (first (reduce p-n [0 s] (sort-by count > (keys numerals))))))
;;   Well, that, uh, that's certainly something there. Horrifyingly dense,
;; really. Can we decompress it just a little?
(fn to-arabic [s]
  (let [numerals {"I" 1, "IV" 4, "IX", 9, "V" 5, "X" 10, "XL" 40, "XC" 90, 
                  "L" 50, "C" 100, "CD" 400, "CM" 900, "D" 500, "M" 1000}
        p-n (fn [[sum num-str] num-token]
              (let [re-token (re-pattern num-token)
                    num-matches (count (re-seq re-token num-str))
                    cleaned-str (clojure.string/join (clojure.string/split num-str re-token))]
                [(+ sum (* num-matches (get numerals num-token))) cleaned-str]))]
    (->> (keys numerals)
         (sort-by count >)
         (reduce p-n [0 s])
         (first))))
;;   If we could import clojure.string, that would actually help with the
;; readability a lot, but hey, 4Clojure. I'm not sure which of these two
;; algorithms I like more, but I'm pretty sure the first is more efficient - not
;; that it matters in the least, since we're limited by the problem statement to
;; 3999.

;;;; 93 - Partially Flatten a Sequence
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/93-wip.clj
;; Original
(fn c-flatten [coll]
  (let [l (first coll) r (next coll)]
    (concat
     (if (sequential? (first l)) (c-flatten l) [l])
     (if (sequential? (first r)) (c-flatten r)))))
;;   So, there was a problem - #28 - which I solved here:
;; https://github.com/MoyTW/4clojure/blob/master/Solutions/21-29.clj
;;   So, the way to partially flatten a sequence? Just look down one level.
;; Hence, (if (sequential (first l)) blah). It's a fairly simple modification.