; Wikipedia implementation
; len_s and len_t are the number of characters in string s and t respectively
; int LevenshteinDistance(string s, int len_s, string t, int len_t)
; {
;   /* test for degenerate cases of empty strings */
;   if (len_s == 0) return len_t;
;   if (len_t == 0) return len_s;
; 
;   /* test if last characters of the strings match */
;   if (s[len_s-1] == t[len_t-1]) cost = 0;
;   else                          cost = 1;
; 
;   /* return minimum of delete char from s, delete char from t, and delete char from both */
;   return minimum(LevenshteinDistance(s, len_s - 1, t, len_t    ) + 1,
;                  LevenshteinDistance(s, len_s    , t, len_t - 1) + 1,
;                  LevenshteinDistance(s, len_s - 1, t, len_t - 1) + cost);
; }

; So. Let's modify it and add a little caching for acceptable speed.

; No cache
(
(fn lev-dist [left right]
  (let [cost (if (= (last left) (last right)) 0 1)]
    (cond 
      (= 0 (count left)) (count right)
      (= 0 (count right)) (count left)
      :else
      (min (inc (lev-dist (butlast left) right))
           (inc (lev-dist left (butlast right)))
           (+ cost (lev-dist (butlast left) (butlast right)))))))
"kitten" "mitten")

; Familiarizing myself with atom
((fn test [n]
  (let [m (atom {})]
    (swap! m assoc :a n)
    @m)) 3)
; Okay, so.

; /w cache first attempt
(
(defn l-d [left right]
  (let [cache (atom {})]
    (letfn [(lev-dist [left right]
              (let [cost (if (= (last left) (last right)) 0 1)
                    key [(count left) (count right)]
                    val
                      (min 
                        (inc (get @cache key (lev-dist (butlast left) right)))
                        (inc (get @cache key (lev-dist left (butlast right))))
                        (+ cost (get @cache key (lev-dist (butlast left) (butlast right)))))]
                (do
                  (swap! cache assoc key val)
                  (cond 
                    (= 0 (count left)) (count right)
                    (= 0 (count right)) (count left)
                    :else val))))]
      (lev-dist left right))))
"a" "a")
                   
; There we go...
(
(defn l-d [left right]
  (let [cache (atom {})]
    (letfn [(lev-dist [left right]
              (let [cost (if (= (last left) (last right)) 0 1)
                    key [left right]]
                (cond 
                  (= 0 (count left)) 
                      (do (swap! cache assoc key (count right)) (count right))
                  (= 0 (count right)) 
                      (do (swap! cache assoc key (count left)) (count left))
                  :else 
                  (let [val 
                          (min 
                            (inc (get @cache key (lev-dist (butlast left) right)))
                            (inc (get @cache key (lev-dist left (butlast right))))
                            (+ cost (get @cache key (lev-dist (butlast left) (butlast right)))))]
                    (do (swap! cache assoc key val) val)))))]
      (lev-dist left right))))
"ki" "si")
      
; Newest but pure shotgun
(
(defn l-d [left right]
  (let [cache (atom {})]
    (letfn [(lev-dist [left right]
              (let [swap-low (fn [k v]
                               (if (> (get @cache k 999) v)
                                 (swap! cache assoc k v)))
                    cost (if (= (last left) (last right)) 0 1)
                    key [(count left) (count right)]]
                (cond 
                  (= 0 (count left)) 
                      (do (swap-low key (count right)) (count right))
                  (= 0 (count right)) 
                      (do (swap-low key (count left)) (count left))
                  :else 
                  (let [val 
                          (min 
                            (inc (get @cache key (lev-dist (butlast left) right)))
                            (inc (get @cache key (lev-dist left (butlast right))))
                            (+ cost (get @cache key (lev-dist (butlast left) (butlast right)))))]
                    (do 
                      (swap-low key val)
                      val)))))]
      (lev-dist left right))))
"ki" "si")

; Funny. Works in the REPL, but...not in 4clojure.