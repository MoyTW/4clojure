;; so this one's pretty easy
;; I didn't know rem was a function, though (remainder)
;; So this is +4 each time
;; (rem 1 4) 1
;; (rem 5 4) 1
;; (rem 9 4) 1

;; Second one gens sequence of +4 (0 4 8 12) then adds one. If resulting < 40, returns.

;; Third - each [x y] pair from (partition 2) is added
;; so it goes
;; 0 1 = 1
;; 2 3 = 5
;; etc etc etc

'(1 5 9 13 17 21 25 29 33 37)