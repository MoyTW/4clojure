;; 4Clojure Problem 34. Implement range
;; url: http://www.4clojure.com/problem/34
(fn custom-range [begin end]
  (loop [begin begin, end end, range-list []]
    (if (= begin end)
        range-list
      (recur (inc begin) end (conj range-list begin)))))