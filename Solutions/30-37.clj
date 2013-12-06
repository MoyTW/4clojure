; -----=====***** 30 (7 lines, 7 total) *****=====-----
#(
  (fn noconsec [in out]
    (if (empty? in)
      out
      (if (= (first in) (first (rest in)))
        (noconsec (rest in) out)
        (noconsec (rest in) (conj out (first in))))
      )
    )
  % [])
; Reformatted
#((fn noconsec [in out]
    (if (empty? in)
      out
      (if (= (first in) (first (rest in)))
        (noconsec (rest in) out)
        (noconsec (rest in) (conj out (first in))))))
  % [])
  
; -----=====***** 31 (9 lines, 16 total) *****=====-----
#(
  (fn noconsec [in out]
    (if (empty? in)
      (reverse out)
      (if (= (first in) (first (first out)))
        (noconsec 
		  (rest in)
          (conj (rest out) (conj (first out) (first in))))
        (noconsec (rest in) (conj out (list(first in)))))
    )
  )
  (rest %) (list (list (first %)))
)
; Reformatted
#((fn noconsec [in out]
    (if (empty? in)
      (reverse out)
      (if (= (first in) (first (first out)))
        (noconsec 
		  (rest in)
          (conj (rest out) (conj (first out) (first in))))
        (noconsec (rest in) (conj out (list(first in)))))))
  (rest %) (list (list (first %))))
  
; -----=====***** 32 (5 lines, 21 total) *****=====-----
(fn dup [in]
  (loop [in-seq in, out-seq []]
    (if (= (first in-seq) nil)
        out-seq
      (recur (rest in-seq) (conj out-seq (first in-seq) (first in-seq) ))
      )
    )
  )
; Reformatted
(fn dup [in]
  (loop [in-seq in, out-seq []]
    (if (= (first in-seq) nil) 
      out-seq
      (recur (rest in-seq) (conj out-seq (first in-seq) (first in-seq) )))))
      
; -----=====***** 33 (10 lines, 31 total) *****=====-----
(fn dup [in, n]
  (loop [in-seq in, out-seq [], cnt n]
    (if (= (first in-seq) nil)
        out-seq
      (recur (rest in-seq) 
             (loop [out-seq out-seq, element (first in-seq), cnt cnt]
               (if (= cnt 0)
                   out-seq
                 (recur (conj out-seq element) element (dec cnt))
                 )
               )
             cnt))))
; Reformatted
(fn dup [in, n]
  (loop [in-seq in, out-seq [], cnt n]
    (if (= (first in-seq) nil) 
      out-seq
      (recur (rest in-seq) 
             (loop [out-seq out-seq, element (first in-seq), cnt cnt]
               (if (= cnt 0) 
                 out-seq
                 (recur (conj out-seq element) element (dec cnt))))
             cnt))))

; -----=====***** 34 (5 lines, 36 total) *****=====-----
(fn custom-range [begin end]
  (loop [begin begin, end end, range-list []]
    (if (= begin end)
      range-list
      (recur (inc begin) end (conj range-list begin)))))

; -----=====***** 35 (1 line, 37 total) *****=====-----      
7 7 7

; -----=====***** 36 (1 line, 38 total) *****=====-----      
[x 7, y 3, z 1]

; -----=====***** 37 (1 line, 39 total) *****=====-----      
"ABC"

; -----=====***** 37 (6 lines, 45 total) *****=====-----      
(fn custom-max [& args]
  (loop [current (first args) inputs (rest args)]
    (if (= (first inputs) nil)
      current
      (recur (if (> (first inputs) current) (first inputs) current )
             (rest inputs)))))