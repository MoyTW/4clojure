; 30
#((fn noconsec [in out]
    (if (empty? in)
      out
      (if (= (first in) (first (rest in)))
        (noconsec (rest in) out)
        (noconsec (rest in) (conj out (first in))))))
  % [])

;   I was on quite the #() binge, wasn't I? To loop/recur:
(fn noconsec [coll]
  (loop [in coll out []]
    (if (empty? in)
      out
      (if (= (first in) (first (rest in)))
        (recur (rest in) out)
        (recur (rest in) (conj out (first in)))))))

;    Still, this is perfect for reduce, so...
(fn noconsec [coll]
  (reduce #(if (= %2 (last %1)) %1 (conj %1 %2)) [] coll))
;   Unsure about whether the if statements in #-defined functions are something
; I should be using - they're a little long and possibly unclear - but then 
; again I use ternary operators in C++/C# without qualms, so...

; 31
#((fn noconsec [in out]
    (if (empty? in)
      (reverse out)
      (if (= (first in) (first (first out)))
        (noconsec 
		  (rest in)
          (conj (rest out) (conj (first out) (first in))))
        (noconsec (rest in) (conj out (list(first in)))))))
  (rest %) (list (list (first %))))
  
;   First things first, just a little general cleaning up 'round the edges
(fn pack-seq [coll]
  (loop [in (rest coll) out (list (list (first coll)))]
    (cond
      (empty? in) (reverse out)
      (= (first in) (ffirst out)) 
        (recur (rest in) (conj (rest out) (conj (first out) (first in))))
      :else (recur (rest in) (conj out (list (first in)))))))

;   So, like most of these, you can use reduce to do it. I wonder if my current
; possible over-usage of reduce will be frowned upon by future-me?
(fn pack-seq [coll]
  (letfn [(p-reduce [out n] 
            (if (= n (ffirst out))
              (conj (rest out) (conj (first out) n))
              (conj out (list n))))]
    (reverse (reduce p-reduce [] coll))))

; 32
(fn dup [in]
  (loop [in-seq in, out-seq []]
    (if (= (first in-seq) nil) 
      out-seq
      (recur (rest in-seq) (conj out-seq (first in-seq) (first in-seq) )))))

;   Again, cleaning up without touching the underlying algorithm. At least I 
; appear to have exited my "Define all of these wtih #!" phase.
;   Actually, it's pretty decent code. The recur is a little long but it's not
; over 80, and so...that (= (first in-seq) nil) bugs me, though.
(fn dup [in]
  (loop [in-seq in, out-seq []]
    (if (empty? in-seq) 
      out-seq
      (recur (rest in-seq) (conj out-seq (first in-seq) (first in-seq))))))
;   There. I'm considering replacing that double (first in-seq) with something
; like (take 2 (repeat (first in-seq))) but that really doesn't get us 
; anywhere, either in terms of length (smaller's better, usually) or 
; actual readability.
;   I'm kinda liking this new comment style, but only for long commentary 
; blocks. For single-line ones, I think I should lose the indent.

; Anyways, we can do a reduce here:
(fn dup-red [in]
  (reduce #(conj %1 %2 %2) [] in))

;   Or, heck, since I'm nattering on about me abusing reduce to an unhealthy
; degree, how about using map?
(fn dup-map [in]
  (mapcat #(vector % %) in))
  
; 33
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
; Oh dear, this is ugly, isn't it?

;   This was written before I knew about closures, so...let's tidy up a bit.
; Again, we're not changing the algorithm, just reformatting it a tad. Oh! And
; here's a good chance to use that (take n (repeat (blah))) thing I was 
; thinking about earlier!
(fn dup [in, n]
  (loop [in-seq in, out-seq []]
    (if (= (first in-seq) nil) 
      out-seq
      (recur (rest in-seq)
             (apply conj out-seq (->> in-seq (first) (repeat) (take n)))))))
             ;(apply conj out-seq (take n (repeat (first in-seq))))))))
             ; which of these is more readable, do you think?

; Of course, we can do it with reduce:
(fn dup-red [in n]
  (reduce #(apply conj %1 (take n (repeat %2))) [] in))

; Or, similarly, a map
(fn dup-map [in n]
  (mapcat #(take n (repeat %)) in))