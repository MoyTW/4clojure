; -----=====***** 21 (5 lines, 5 total) *****=====-----
(fn cust-nth [coll n]
  (loop [coll coll n n]
    (if (= n 0)
      (first coll)
      (recur (rest coll) (dec n)))))

; -----=====***** 22 (5 lines, 10 total) *****=====-----
; This is my old code - from what I was just starting out.
; See, what I did was burn through the first twenty-ish problems before realizing I should make an account.
; So here's the first taste of how bad it looked before I wised up about the formatting.
#( 
  (fn rec [l i] 
    (if 
      (= (first l) nil)
      i
      (rec (rest l) (+ i 1))
      )
    )
  % 0
  )
; Reformatted a tad. Really should *not* be in anonymous function like that, but I'm not changing the code, just reformatting it.
#((fn rec [l i] 
    (if (= (first l) nil)
      i
      (rec (rest l) (+ i 1))))
  % 0)

; -----=====***** 23 (5 lines, 15 total) *****=====-----
; Original code
#(
  (fn rev [src, dest]
    (if 
      (= (first src) nil)
      dest
      (rev 
       (rest src) 
       (conj dest (first src))
      )
     )
    )
  % '()
)
; Reformatted code.
#((fn rev [src, dest]
    (if (= (first src) nil)
      dest
      (rev (rest src) (conj dest (first src)))))
  % '())

; -----=====***** 24 (5 lines, 20 total) *****=====-----
; What's with everything being an anonymous function?
#(
  (fn sum [sq, sm]
    (if 
      (= nil (first sq))
         sm
      (sum
       (rest sq) (+ sm (first sq))
      )
    )
  )
  % 0
)
; Reformatted.
#((fn sum [sq, sm]
    (if (= nil (first sq))
      sm
      (sum (rest sq) (+ sm (first sq)))))
  % 0)

; -----=====***** 25 (7 lines, 22 total) *****=====-----
#(
  (fn odd [src, dest]
    (if (= (first src) nil)
      dest
      (if (= 1 (mod (first src) 2))
        (odd (rest src) (conj dest (first src)))
        (odd (rest src) dest)
       )
     )
   )
  %, []
)
; Reformatted
#((fn odd [src, dest]
    (if (= (first src) nil)
      dest
      (if (= 1 (mod (first src) 2))
        (odd (rest src) (conj dest (first src)))
        (odd (rest src) dest))))
  %, [])

; -----=====***** 26 (10 lines, 32 total) *****=====-----
; Original, comment is in original code
#(
  ; Hacky! Doesn't handle fib(1) or fib(2)!
  (fn fib [n, sq]
    (if (= n 0)
      sq
      (fib 
       (- n 1) 
       (conj sq (+ 
        (nth sq (- (count sq) 2 ))
        (last sq))
       )
      )
    )
  )
  (- % 2) [1 1]
)
; Reformatted
; Hacky! Doesn't handle fib(1) or fib(2)!
#((fn fib [n, sq]
    (if (= n 0)
      sq
      (fib 
       (- n 1) 
       (conj sq 
             (+ 
              (nth sq (- (count sq) 2 ))
              (last sq))))))
  (- % 2) [1 1])

; -----=====***** 27 (1 line, 33 total) *****=====-----  
(fn [x] (= (seq x) (reverse x)))

; -----=====***** 28 (5 lines, 38 total) *****=====-----  
(fn c-flatten [coll] 
  (let [l (first coll) r (next coll)]
    (concat 
     (if (sequential? l) (c-flatten l) [l])
     (if (sequential? r) (c-flatten r)))))

; -----=====***** 29 (2 lines, 40 total) *****=====-----       
(fn [in]
   (clojure.string/join 
    (filter #(Character/isUpperCase %) (seq in))
    )
  )
; Reformatted
(fn [in]
  (clojure.string/join (filter #(Character/isUpperCase %) (seq in))))