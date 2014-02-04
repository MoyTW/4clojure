;; Note: I accidentally kind of messed up! In the file 112-115, it actually goes up to 116! However, I'm going to do 114 + 115 here, ignoring 116 - I'll pick that up in 116-118 (which in reality will be 116+118 and 117, since 117 is huge).

;;;; 114 - Global take-while
;;; Scratch: https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/114-wip.clj
;; Original:
(fn take-before-nth-match [n pred coll]
  (letfn [(count-to-take [n c coll]
            (cond
              (= n 0) c
              (pred (first coll)) (count-to-take (dec n) (inc c) (rest coll))
              :else (count-to-take n (inc c) (rest coll))))]
    (take (dec (count-to-take n 0 coll)) coll)))
;; So, this is a counter-based sort of take-while. Basically, you need to find the nth member of the sequence for which the given pred is true, and then take everything before that (but not including that). I kind of don't like n-based problems because, invariably, the solutions are n-based and require old-style recursion. Not that old-style recursion is bad! But whenever I drop down into recursion I have the nagging feeling that I'm not transforming and it kind of bugs me.
;; Anwyays, the above description - find the nth member of the sequence, and then take everything before that - is what I did up top. I looked at it for a bit, thinking about how to rewrite it, and well, first? Ain't no reason not to be using recur, here. If you can replace bald recursion with recur without rewriting anything, always do it. That's pretty much an unmitigated good, there. So:
(fn take-before-nth-match [n pred coll]
  (letfn [(count-to-take [n c coll]
            (cond
              (= n 0) c
              (pred (first coll)) (recur (dec n) (inc c) (rest coll))
              :else (recur n (inc c) (rest coll))))]
    (take (dec (count-to-take n 0 coll)) coll)))
;; Anyways, what is this doing? Well, it uses count-to-take to find out how many it wants to take, and then takes those and returns those. count-to-take basically walks along the target collection, one at a time, keeping track of how many steps it's moved. Every time it finds a member for which the predicate is true, it decrements n (which counts how many true things we want to pass); once it reaches zero, it returns the count.
;; Oh, and you have to decrement it at some point, because you don't actually want the nth targeted member, you want everything before it. That could either go in count-to-take, by decrementing c before you return, or before the take. Where would it be least confusing? Oh, and unrelated; (= n 0) should be (zero? n). How about:
(fn take-before-nth-match [n pred coll]
  (letfn [(count-to-take [n c coll]
            (cond
              (zero? n) (dec c)
              (pred (first coll)) (recur (dec n) (inc c) (rest coll))
              :else (recur n (inc c) (rest coll))))]
    (take (count-to-take n 0 coll) coll)))
;; Well, that's all nice and good, but it's basically the same algorithm, right!? Right. So, I started looking for a good way to rewrite this. I figured I had a good one with take-while (well, split-with, actually - but that uses take-while!) for a minute there:    
(fn take-before-nth [n pred coll]
  (letfn [(take-next [n coll]
            (if (zero? n)
                nil
                (let [[v r] (split-with pred coll)]
                  (concat (conj (into [] v) (first r)) (take-next (dec n) (rest r))))))]
    (take-next (dec n) coll)))
;; It's basically the same thing, except you're directly building the targeted sequence instead of counting how many you want to take and taking those. Unfortunately, it failed because split-with assumes truth at the start. I briefly considered partition-by, but then figured that would actually end up more complex, since partition-by actually works on changes, not values.
;; How about other approaches? Well, how about literally just finding the position of the nth member for which pred is true, and then splitting on that? For example,
(fn uses-filter [n pred coll]
  (take (.indexOf coll (nth (filter pred coll) (dec n))) coll))
;; It works, but it's ugly and more than a little hacky. Also there are, like, three instances of coll in one line there. Hold on, let me reformat this:
(fn uses-filter [n pred coll]
  (take (.indexOf coll 
                  (nth (filter pred coll) 
                       (dec n))) 
        coll))
;; Hoo boy that's ugly, and I don't have any good idea of how I might un-ugly it. Threading macros are right out; it doesn't have quite enough form to make those worthwhile. Ackpth! Well, it works, so I guess I'll just leave it here as a valid if terrible alternate solution and hope nobody looks at it.
;; Maybe I should've just left it in one-liner form; it at least has the advantage of compactness then...

;;;; 115 - The Balance of N
;;; Scratch: https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/114-wip.clj
;; Original:
(fn compare-side-digits [x]
  (let [s (map #(Integer. (str %)) (str x))
        l-c (int (/ (count s) 2))
        r-c (Math/ceil (/ (count s) 2))]
    (= (apply + (take l-c s))
       (apply + (drop r-c s)))))
;; This is yet another one of those problems where converting to and from a string basically solves the whole problem very easily and quickly. So, basically, all you need to do is turn the number into a string, split the string up into a sequence of individual integers, and then add the left and right halves of the sequence up.
;; You might think that you could simplify the code with partition or split-at, but if there are an uneven number of digits in the number, that very quickly becomes nonviable. You can kind of compress it by moving some of the stuff in the let into the comparison calculation:
(fn compare-side-digits [x]
  (let [s (map #(Integer. (str %)) (str x))]
    (= (apply + (take (int (/ (count s) 2)) s))
       (apply + (drop (Math/ceil (/ (count s) 2)) s)))))
;;   but I'm not sure that actually improves readability. How about:
(fn compare-side-digits [x]
  (let [s (map #(Integer. (str %)) (str x))
        halfway (/ (count s) 2)]
    (= (apply + (take (Math/floor halfway) s))
       (apply + (drop (Math/ceil halfway) s)))))
;; It at least makes clear what's going on with the floor/ceil this way! There's not a lot of rewriting here because I'm really mostly satisfied with how I did it. So...to the Internets to see what other tricks I've missed!
;;   Oh, this one's nice! From: https://gist.github.com/syohsue/1963600
(fn [num]
  (let [coll (map #(int %) (str num))
        len (Math/floor (/ (count coll) 2))]
    (= (apply + (take len coll))
       (apply + (take-last len coll)))))
;; He very neatly solved the whole "Floor or ceil?" by just using take-last, which I didn't even know existed. Another way to square the particular problem away at http://pastebin.com/3BJpUjgr:
(fn [n]
  (let [s (str n)
        c (quot (count s) 2)
        l (map read-string (re-seq #"." s))]
    (= (apply + (take c l)) (apply + (take c (reverse l))))))
;; Instead of mucking around with take-last or floor/ceil, this person just reversed the sequence and took half the count from the reversed sequence. More than one way to break an egg, right?
;;   Err, that is how that saying goes, isn't it?