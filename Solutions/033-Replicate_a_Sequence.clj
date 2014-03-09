;; 4Clojure Problem 33. Replicate a Sequence
;; url: http://www.4clojure.com/problem/33
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