;; 4Clojure Problem 40. Interpose a Seq
;; url: http://www.4clojure.com/problem/40
; This is just hilariously hacky!
(fn custom-interpose [sep, in-seq]
  (butlast (flatten 
   (map (fn [in, sep] [in sep]) in-seq (take (count in-seq) (repeat sep))))))