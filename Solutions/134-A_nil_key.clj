;; 4Clojure Problem 134. A nil key
;; url: http://www.4clojure.com/problem/134
(fn __ [k m]
  (= nil (get m k :not-present)))