; Hmm. Okay. So.
; I...don't know what (range 0 100 7/6) is. Oh, the last parameter is the step.
; First thing here is what are the assumptions that we can make?
; We can assume that the problem is always solvable (that what gets passed in does, in fact, have at least one common number). It also looks like all the sequences are increasing. Finally, they may or may not be infinite.
; So, given that the sequences are always increasing...
; How about, we bump the lowest one upwards until they're all equal?

;baby steps
(defn lazy-search [& seqs]
  (if (= 1 (count seqs)) (ffirst seqs)
    (loop [& seqs]
      (let [seq-vec (into [] seqs)
            firstvals (map first seq-vec)]
        (if (apply = firstvals) (first firstvals)
          :notallequal)))))
        
(defn lazy-search [& seqs]
  (if (= 1 (count seqs)) (ffirst seqs)
    (loop [& seqs]
      (let [firstvals (map first seqs)
            low-val (apply min firstvals)]
        (if (apply = firstvals) low-val
          (map #(if (= low-val (first %)) (rest %) %) seqs))))))
          ;(recur (map #(if (= low-val (first %)) (rest %) %) seqs)))))))