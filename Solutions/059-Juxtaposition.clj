;; 4Clojure Problem 59. Juxtaposition
;; url: http://www.4clojure.com/problem/59
(fn cust-jux
  ([x] (fn [& args] (list (apply x args))))
  ([x y] (fn [& args] (list (apply x args) (apply y args))))
  ([x y z] (fn [& args] (list (apply x args) (apply y args) (apply z args)))))