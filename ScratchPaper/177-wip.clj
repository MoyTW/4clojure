;; I should use instaparse for this! Too bad that's not something I can do.
;; Okay, so. How do. Well, if all we need to do is match them we can use a stack.
;; wait clojure doesn't have stacks
;; I think (conj) and (pop) give the same behavior though?

(pop (conj [] :a :b))

;; Ok, yeah, can totally do.

(def symbol-matches {\[ \], \( \), \{ \}})

(defn r [stack n]
  (if (= n (symbol-matches (peek stack)))
      (pop stack)
      (conj stack n)))

(defn __ [s]
  (empty? (reduce r [] s)))
      
(__ "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))")
(__ "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))")
(__ "(start, end]")

;; oh obv needs to handle other chars
(defn r [stack n]
  (cond
    (= n (symbol-matches (peek stack))) (pop stack)
    (symbol-matches n) (conj stack n)
    stack))

(defn __ [s]
  (empty? (reduce r [] s)))
      
(__ "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))")
(__ "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))")
(__ "(start, end]")

(defn __ [s]
  (let [parens-matches {\[ \], \( \), \{ \}}
        parens #{\[ \], \( \), \{ \}}]
    (empty? (reduce (fn [stack n]
                      (cond
                        (= n (parens-matches (peek stack))) (pop stack)
                        (parens n) (conj stack n)
                        :else stack))
                    []
                    s))))
(__ "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))")
(__ "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))")
(__ "(start, end]")