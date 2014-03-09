;; 4Clojure Problem 85. Power Set
;; url: http://www.4clojure.com/problem/85
(fn power-set [trgt-st]
  (letfn [(break-up [st]
                    (reduce #(conj %1 (disj st %2)) #{} st))
          (next-line [st]
                     (reduce #(apply conj %1 (break-up %2)) #{} st))]
    (loop [st #{trgt-st} pwr-st #{trgt-st}]
      (if (= st #{#{}}) pwr-st
        (recur (next-line st) (apply conj pwr-st (next-line st)))))))