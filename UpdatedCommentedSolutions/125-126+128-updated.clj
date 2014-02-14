 ;;; 127 is quite, and 130 ain't just a line, so I'm knocking off everything in;;   between.

;;;; 125 - Gus' Quinundrum
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/125-wip.clj
;; Original:
(fn [] (let [x ["(fn [] (let [x " "] (str (first x) x (last x))))"]] (str (first x) x (last x))))
;;   So, this is the one that made me stop doing 4Clojure for a while because I
;; got stuck on it and then started writing a parser and sort of just dropped
;; off with the 4Clojure while I was doing that.
;;   Initially, I figured that using quotes to solve this would be the best way
;; but I really couldn't quite get it to work and then I gave up. When I came
;; back to do more 4Clojure, I basically said, "Screw quotes, I'll just copy
;; whatever it was that the Java example is doing!"
;;   So, the idea is you have a string, by breaking the string up into a
;; "Everything before this string" and "Everything after this string," you can
;; return the function by printing everything before the string, the string
;; itself, and then everything after that string. Originally, I was trying to
;; actually map each of these in a let binding but that proved troublesome
;; because you can't actually print the while string itself that way, because
;; there are characters inside the let.
;; So I ended up stuffing the strings into a vector.
;;   I have no idea how other people did this, and I suspect that there is at
;; least one beautiful and elegant usage of the quoting macro that will make me
;; feel massively inadequate. To the Googles!
;;   From
;; https://github.com/youz/4clojure-golf/blob/master/125_Gus%27%20Quinundrum.clj
;; we have the most blatent abuse of commas, ever:
(fn [x] (str x x))
'(fn [x] (str x x))
;;   While this satisfies the unit tests, it *doesn't* satisfy the description of
;; the problem. One, it's not a function, it's a function and a value that will
;; cleverly be passed into itself when called. Two, it takes a parameter. Three,
;; when called like ((fn [x] (str x x))'(fn [x] (str x x))) the string it
;; outputs is missing the quote! It's clever as all hell, though, I will admit.
;; It exploits the way that the test is written, because the function is quoted
;; and then the two quoted functions are passed into str - which also drops the
;; quotes.
;;   It's an amazingly egregious case of loophole abuse. It's pretty great.
;;   From https://github.com/jimm/clojure/blob/master/4clojure.clj we have...hey,
;; that's basically mine:
(fn [] (let [a ["(fn [] (let [a " "] (str (a 0) a (a 1))))"]] (str (a 0) a (a 1))))
;;   I swear that I did not steal that. It's basically the same thing, except
;; that uh, what's that apply doing? That's...completely unnecessary. Also, it
;; uses indices to access the string array instead of first/last, but those are
;; minor differences - they're basically the same as mine! Huh. Also he said it
;; wasn't his in a comment, so...I wonder whose it is?
;;   From https://gist.github.com/anonymous/1255743 we have...uh, that must be
;; where the guy who said it wasn't his got it from, because it's exactly the
;; same, sans formatting:
(fn []
  (let [a ["(fn [] (let [a "
           "] (apply str (a 0) a (a 1))))"]] (apply str (a 0) a (a 1))))
;;   From: http://pastebin.com/gQv7i60S we have another variation, this time
;; using format!
(fn [] (let [x "(fn [] (let [x %s] (format x (pr-str x))))"] (format x (pr-str x))))
;;   That's...hey, that's pretty interesting, actually. It's inserting x, using
;; pr-str to make sure it keeps the quotes, into x using format. That's a
;; different approach! I like it the most, actually.

;;;; 126 - Through the Looking Class
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/126-wip.clj
;; Original:
java.lang.Class
;;   What class' class is equal to itself and is not nil? Well, class' class is
;; equal to itself.

;;;; 128 - Recognize Playing Cards
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/128-wip.clj
;; Original:
(fn resolve-card [[suit rank]]
  (let [suits {\D :diamond \H :heart \C :club \S :spade}
        ranks {\2 0 \3 1 \4 2 \5 3 \6 4 \7 5 \8 6 \9 7 \T 8 \J 9 \Q 10 \K 11 \A 12}]
    {:suit (get suits suit) :rank (get ranks rank)}))
;;   This is pretty easy - all it is is a mapping into two, uh, maps. You could
;; write it with ifs and conds and oh dear, but I think that this is actually
;; perfectly elegant and beautiful as it is already! I can barely bear to modify
;; it. I guess I could get rid of the gets, though. Eeeh, why not:
(fn resolve-card [[suit rank]]
  (let [suits {\D :diamond \H :heart \C :club \S :spade}
        ranks {\2 0 \3 1 \4 2 \5 3 \6 4 \7 5 \8 6 \9 7 \T 8 \J 9 \Q 10 \K 11 \A 12}]
    {:suit (suits suit) :rank (ranks rank)}))
;;   I wonder how other people solved it?
;;   From 4Clojure golf (the last one, actually) -
;; https://github.com/youz/4clojure-golf/blob/master/128_Recognize%20Playing%20Cards.clj
;; -
(fn [[s n]]
  {:suit ({\D :diamond \H :heart} s :club)
   :rank (.indexOf "23456789TJQKA" (int n))
   })
;;   How tricksy! You'll notice that there's no :spade anywhere in this code.
;; That's because none of the unit tests use :spade, and because it's
;; golf...tricksy, tricksy. Also, that indexOf is pretty genius.
;;   From
;; https://github.com/qiuxiafei/4clojure/blob/master/answers/128.%20Recognize%20Playing%20Cards
;; comes a function from somebody who got all the way to 128 without reading
;; anybody else's code (else why would the formatting be like this?):
(fn poke [s]
  (let [suit-map {\H :heart, \C :club, \D :diamond, \S :spades}
        rank-map {\2 0, \3 1, \4 2, \5 3, \6 4, \7 5,
                  \8 6, \9 7, \T 8, \J 9, \Q 10, \K 11, \A 12}
        ]
    {:suit (suit-map (first s)), :rank (rank-map (last s))}
  ))
;;   That's...like, the same thing as mine. Hurmm, way to make me feel
;; unoriginal. At least mine is prettier and has better formatting!
;; From somebody who likes condp: http://pastebin.com/DJprrW2H
(fn recognize-playing-cards [card]
  (let [suit #(condp = % \S :spade \H :heart \D :diamond \C :club)
        rank #((apply hash-map (interleave "23456789TJQKA" (range))) %)]
    {:suit (suit (first card))
     :rank (rank (second card))}))
;;   That's a particularly clever usage of interleave, which I hadn't thought of,
;; but that's dang elegant. If only he dropped the condp and destructured card!
;; I like it so much I think I'm going to steal it:
(fn resolve-card [[suit rank]]
  (let [suits {\D :diamond \H :heart \C :club \S :spade}
        ranks (apply hash-map (interleave "23456789TJQKA" (range)))]
    {:suit (suits suit) :rank (ranks rank)}))