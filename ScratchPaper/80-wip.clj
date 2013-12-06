; hmm
(filter #(= 0 (mod 8128 %)) (take 8128 (range)))

; lol zero-div
(filter #(= 0 (mod 8128 %)) (rest (take 8128 (range))))

(apply + (filter #(= 0 (mod 8128 %)) (rest (take 8128 (range)))))

;ezpz
(defn is-perfect [n]
  (apply + (filter #(= 0 (mod n %)) (rest (take n (range))))))

; that was fast, did I do it wrong?
(= false (is-perfect 5))
(= true (is-perfect 6))

; wait yes I did I'm dumb and careless
(defn is-perfect [n]
  (= n (apply + (filter #(= 0 (mod n %)) (rest (take n (range)))))))

; hooray  
(= true (is-perfect 28))

; For 4Clojure
(fn [n] (= n (apply + (filter #(= 0 (mod n %)) (rest (take n (range)))))))

; I don't know how to format.
(fn [n] 
  (= n (apply 
        + 
        (filter #(= 0 (mod n %)) 
                (rest (take n (range)))))))