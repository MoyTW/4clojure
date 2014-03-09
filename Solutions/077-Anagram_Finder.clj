;; 4Clojure Problem 77. Anagram Finder
;; url: http://www.4clojure.com/problem/77
(fn anag [coll]
  (reduce #(conj %1 %2) #{}
          (filter
           #(> (count %) 1)
           (vals 
            ((fn to-map [coll] 
               (reduce 
                (fn [m n]
                  (let [k (apply conj #{} n)] 
                    (assoc m k (conj (get m k #{}) n)))) 
                {} 
                coll)) 
             coll)))))