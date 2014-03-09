;; 4Clojure Problem 113. Making Data Dance
;; url: http://www.4clojure.com/problem/113
(fn [& args]
  (reify
    java.lang.Object
    (toString [_]
      (clojure.string/join ", " (sort args)))
    clojure.lang.ISeq
      (seq [_]
        (if (empty? args) nil
          (distinct args)))))