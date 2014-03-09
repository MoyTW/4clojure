;; 4Clojure Problem 65. Black Box Testing
;; url: http://www.4clojure.com/problem/65
(fn check-type [coll]
  (let [coll-m (conj coll {:w :x :y :z})]
	(if (= (+ 2 (count coll)) (count coll-m))
	    :map
      (let [coll-c (conj coll :z :z :x)]
        (if (= (+ 2 (count coll)) (count coll-c))
	      :set
	      (if (= (last coll-c) :x)
	          :vector
	        :list))))))