;; ohgod it has a huge list of restrictions what is this
;; All right, so:
;; With just function, infinite matrix
;; With function and 2 arguments, you drop (uh-oh) the first n cols and m rows
;; With function and 4 arguments, you take the first two and do the previous, then you take s, t, to make a finite box

;; Okay I think I have a good idea of how it's going to work. See, I'd...totally use for here but uh guess what you can't
;; I *think* what it wants you to do is use manual recursion with laziness and all that. Or, rather, the problem was written in such a way as to encourage...etc, etc (it's a problem it can't want anything it's not an animate thing!)

;; SO. What's the plan? Well, first - get the base "infinite matrix" thing down, so it'll work with just f.
;; Generate [0...] for x, y, and apply the function. That...that actually seems pretty simple.

;; To start, roll custom version of range:
;; Behavior: 
;; (ascending) -> (0 1 2 3 4 5)
;; (ascending 3) -> (3 4 5 6 7 8)
;; (ascending 3 5) -> (3 4)
(defn ascending
  ([] (ascending 0))
  ([start]
    (lazy-seq (cons start (ascending (inc start)))))
  ([start end]
    (take (- end start) (ascending start))))
;; The language-standard versions are all tricked out of course but this'll do for now.
(take 5 (ascending))
(take 10 (ascending 4))
(ascending 3 5)

(defn build-row [f r start]
  (let [a (ascending start)]
    (map #(f r %) a)))
(take 5 (build-row str 3))

(defn __ [f]
  (let [a (ascending 0)]
    (map #(build-row f % 0) a)))
  
(take 5 (map #(take 6 %) (__ str)))

;; With two extra vars
(defn __ 
  ([f]
    (let [a (ascending 0)]
      (map #(build-row f % 0) a)))
  ([f m n]
    (let [a (ascending m)]
      (map #(build-row f % n) a))))
(= (take 6 (map #(take 5 %) (__ str 3 2)))
   [["32" "33" "34" "35" "36"]
    ["42" "43" "44" "45" "46"]
    ["52" "53" "54" "55" "56"]
    ["62" "63" "64" "65" "66"]
    ["72" "73" "74" "75" "76"]
    ["82" "83" "84" "85" "86"]])

;; Okay, let's do 5:
(defn __
  ([f]
    (let [a (ascending 0)]
      (map #(build-row f % 0) a)))
  ([f m n]
    (let [a (ascending m)]
      (map #(build-row f % n) a)))
  ([f m n s t]
    (let [infinite (__ f m n)]
      (take s (map #(take t %) infinite)))))
(= (__ * 3 5 5 7)
   [[15 18 21 24 27 30 33]
    [20 24 28 32 36 40 44]
    [25 30 35 40 45 50 55]
    [30 36 42 48 54 60 66]
    [35 42 49 56 63 70 77]])

;; Seems pretty simple, I'm not sure why so many people seem to have not solved it (count's at only 239 at this time!). 
;; Incidentally, "Love Triangle" the one I did my horrible horrible hack for is only solved by 119. It'd be more accurately said 118 since you can hardly say that I solved it.
;; Wait. How do I get build-row into...uhhhh. I can't just letfn it can I, since the dispatches have to be - hmm. Okay, a little reshuffling I guess:
(fn __
  ([f]
    (__ f 0 0))
  ([f m n]
    (letfn [(ascending 
              ([start]
                (lazy-seq (cons start (ascending (inc start)))))
              ([start end]
                (take (- end start) (ascending start))))
            (build-row [f r start]
              (let [a (ascending start)]
                (map #(f r %) a)))]
    (let [a (ascending m)]
      (map #(build-row f % n) a))))
  ([f m n s t]
    (let [infinite (__ f m n)]
      (take s (map #(take t %) infinite)))))