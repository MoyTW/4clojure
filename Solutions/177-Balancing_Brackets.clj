;; 4Clojure Problem 177. Balancing Brackets
;; url: http://www.4clojure.com/problem/177
(fn __ [s]
  (let [parens-matches {\[ \], \( \), \{ \}}
        parens #{\[ \], \( \), \{ \}}]
    (empty? (reduce (fn [stack n]
                      (cond
                        (= n (parens-matches (peek stack))) (pop stack)
                        (parens n) (conj stack n)
                        :else stack))
                    []
                    s))))