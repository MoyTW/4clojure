;; 4Clojure Problem 32. Duplicate a Sequence
;; url: http://www.4clojure.com/problem/32
(fn dup [in]
  (loop [in-seq in, out-seq []]
    (if (= (first in-seq) nil)
        out-seq
      (recur (rest in-seq) (conj out-seq (first in-seq) (first in-seq) ))
      )
    )
  )