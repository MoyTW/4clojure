; 26
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
; Okay what's up with this formatting?

#((fn fib [n, sq]
    (if (= n 0)
      sq
      (fib (- n 1) (conj sq (+ (nth sq (- (count sq) 2 ))(last sq))))))
  (- % 2) [1 1])
; not much better

; Using loop/recur
(fn fib [n]
  (loop [n (- n 2)
         sq [1 1]]
    (if (= n 0) sq
      (recur (dec n) (conj sq (+ (nth sq(- (count sq) 2)) (last sq)))))))
      
; That's kinda ugly. Can we use a higher-level construct?
; Well, how about iterate? New sequence is last two members of old
(defn fib-iter [x]
  (conj x (apply + (take-last 2 x))))

(fn fib-iter [x]
  (last (take (dec x) (iterate #(conj % (apply + (take-last 2 %))) [1 1]))))
; This works but a) it's ugly and b) it makes a gazillion sequences!

; 27
(fn [x] (= (seq x) (reverse x)))

; 28
(fn c-flatten [coll] 
  (let [l (first coll) r (next coll)]
    (concat 
     (if (sequential? l) (c-flatten l) [l])
     (if (sequential? r) (c-flatten r)))))
     
; 29
(fn [in]
  (clojure.string/join (filter #(Character/isUpperCase %) (seq in))))