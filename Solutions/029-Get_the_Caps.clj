;; 4Clojure Problem 29. Get the Caps
;; url: http://www.4clojure.com/problem/29
(fn [in]
   (clojure.string/join 
    (filter #(Character/isUpperCase %) (seq in))
    )
  )