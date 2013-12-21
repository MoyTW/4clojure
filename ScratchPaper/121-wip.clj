;;   Now this is interesting.
;; So, we're getting a list passed in. And we need to build an evaluator? Well, that's actually pretty simple...but returning a function that takes a map which holds the values, I wonder how to do that? Well, we could do a sorta lookup thing; if it's a character that's not one of the four operations, look it up in the map for its value.
;; Anyways, the eval is recursive. So. Basically: take 3, if second of third are lists, recursively call, if they're numbers, eval, if they're characters, lookup.

(
(defn e [[op p1 p2]]
  (letfn [(e-p [p]
            (cond
              (list? p) (e p)
              (number? p) p
              :else 0))] ; Lookup goes here!
    ((eval op) (e-p p1) (e-p p2))))
'(/ 5 (* 3 2)))
;; So, this handles normal evaluation (as in, it doesn't substitute things in). We could do a closure over the map, like so:
(defn e-map [l m]
  (let [e (fn [[op p1 p2]]
            (letfn [(e-p [p]
                      (cond
                        (list? p) (e p)
                        (number? p) p
                        :else (m p)))]
              ((eval op) (e-p p1) (e-p p2))))]
    (e l)))
(e-map '(+ 5 (* 3 9)) '{a 2})
(e-map '(+ 5 (* 3 a)) '{a 2})

;; Weird. It's...losing the closure on the second layer in? What I mean, is, the following does work:
(e-map '(+ a (* 3 9)) '{a 5}) ; = 32
;; but if we do
(e-map '(+ 5 (* 3 a)) '{a 9}) ; = 5
;; it breaks! I didn't know that was how it worked! Okay, let's try pulling e-p out into e-map's letfn:
(defn e-map [l m]
  (letfn [(e-p [p]
            (cond
              (list? p) (e p)
              (number? p) p
              :else (m p)))
          (e [[op p1 p2]]
            ((eval op) (e-p p1) (e-p p2)))]
    (e l)))
(= 32 (e-map '(+ a (* 3 9)) '{a 5}))
(= 32 (e-map '(+ 5 (* 3 a)) '{a 9}))

;; I had no idea that happened! Okay, so that's really good to know. With that in mind, we can go on to create our function:
((
(fn return-eval [l]
  (fn e-map [m]
    (letfn [(e-p [p]
              (cond
                (list? p) (e p)
                (number? p) p
                :else (m p)))
            (e [[op p1 p2]]
              ((eval op) (e-p p1) (e-p p2)))]
      (e l))))
'(/ a b))
'{b 8 a 16})
;; oh wait 4Clojure doesn't let us use eval welp
;; okay how about resolve
(fn return-eval [l]
  (fn e-map [m]
    (letfn [(e-p [p]
              (cond
                (list? p) (e p)
                (number? p) p
                :else (m p)))
            (e [[op p1 p2]]
              ((resolve op) (e-p p1) (e-p p2)))]
      (e l))))
;; oh wait 4Clojure doesn't let us use resolve welp
;; No, seriously. What's the intended way to get to the actual function from the quoted input? Hmm. I have no friggin' idea how to do this without utilizing eval or resolve or some other namespace-y function!
;; Actuaaaaaaly. Hmm. You could...you could eval the whole thing? No, wait. Hmm. If you did that it'd look for things in the namespace but it's in the map, not the namespace.
;;   Aww, screw't.
((
(fn return-eval [l]
  (fn e-map [m]
    (letfn [(e-p [p]
              (cond
                (list? p) (e p)
                (number? p) p
                :else (m p)))
            (e [[op p1 p2]]
              (cond
                (= op '*) (* (e-p p1) (e-p p2))
                (= op '+) (+ (e-p p1) (e-p p2))
                (= op '-) (- (e-p p1) (e-p p2))
                (= op '/) (/ (e-p p1) (e-p p2))))]
      (e l))))
'(+ a b 2))
'{a 2 b 4})
;; Hmm. Okay. I note that unit test 2 passes in three arguments to the quoted form. I can deal with that, but...this thing's getting cruftier all the time.
((
(fn return-eval [l]
  (fn e-map [m]
    (letfn [(e-p [p]
              (cond
                (list? p) (e p)
                (number? p) p
                :else (m p)))
            (e [[op & args]]
              (cond
                (= op '*) (apply * (map e-p args))
                (= op '+) (apply + (map e-p args))
                (= op '-) (apply - (map e-p args))
                (= op '/) (apply / (map e-p args))))]
      (e l))))
'(+ a b 2))
'{a 2 b 4})
;; Actually, I think that cleaned it up. We could actually clean it up a little more by putting the (map e-p args) into a let, instead of repeating it four times, but...no, actually, let's do that. It's not like this isn't hilariously long already.
((
(fn return-eval [l]
  (fn e-map [m]
    (letfn [(e-p [p]
              (cond
                (list? p) (e p)
                (number? p) p
                :else (m p)))
            (e [[op & args]]
              (let [e-args (map e-p args)]
                (cond
                  (= op '*) (apply * e-args)
                  (= op '+) (apply + e-args)
                  (= op '-) (apply - e-args)
                  (= op '/) (apply / e-args))))]
      (e l))))
'(+ a b 2))
'{a 2 b 4})