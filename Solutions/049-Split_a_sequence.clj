;; 4Clojure Problem 49. Split a sequence
;; url: http://www.4clojure.com/problem/49
(fn [x coll]
  (loop [n x in-coll coll out-coll []]
    (if (= n 0)
	    (conj [out-coll] in-coll)
      (recur (dec n) (subvec in-coll 1) (conj out-coll (first in-coll))))))