; 22
#((fn rec [l i] 
    (if (= (first l) nil)
      i
      (rec (rest l) (+ i 1))))
  % 0)
  
; using loop/recur
(fn cust-count [s]
  (loop [s s i 0]
    (if (= (first s) nil)
      i
      (recur (rest s) (inc i)))))

; using reduce
(fn cust-count [s]
  (reduce (fn [c n] (inc c)) 0 s))
  
; 23
#((fn rev [src, dest]
    (if (= (first src) nil)
      dest
      (rev (rest src) (conj dest (first src)))))
  % '())
  
; ugh, okay, use reduce
(fn cust-rev [s]
  (reduce #(conj %1 %2) '() s))
  
; 24
#((fn sum [sq, sm]
    (if (= nil (first sq))
      sm
      (sum (rest sq) (+ sm (first sq)))))
  % 0)

; again, reduce
(fn sum-all [s]
  (reduce + s))
  
; 25
#((fn odd [src, dest]
    (if (= (first src) nil)
      dest
      (if (= 1 (mod (first src) 2))
      
        (odd (rest src) (conj dest (first src)))
        (odd (rest src) dest))))
  %, [])
  
; using reduce
(fn odd [s]
  (reduce #(if (= 1 (mod %2 2)) (conj %1 %2) %1) [] s))

; using filter
(fn odd [s]
  (filter #(= 1 (mod % 2)) s))