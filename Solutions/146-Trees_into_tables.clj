;; 4Clojure Problem 146. Trees into tables
;; url: http://www.4clojure.com/problem/146
(fn __ [m]
  (apply merge
         (for [kv m
               v (second kv)
               :let [key (first kv)
                     [fv rv] v]]
           {[key fv] rv})))