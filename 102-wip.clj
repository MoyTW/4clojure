; ...looks easy enough. Split on -, capitalize all but first, shove them back together.

(
(fn cap-caml [lead & words]
  (apply str lead (map clojure.string/capitalize words)))
"zero" "blue" "hero")

(fn camel-case [word]
  (let [[lead & words] (clojure.string/split word #"-")]
    (apply str lead (map clojure.string/capitalize words))))