;; 4Clojure Problem 102. intoCamelCase
;; url: http://www.4clojure.com/problem/102
(fn camel-case [word]
  (let [[lead & words] (clojure.string/split word #"-")]
    (apply str lead (map clojure.string/capitalize words))))