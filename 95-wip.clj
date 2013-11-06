; Okay, let's see. Can we do something uniquely clojue-like? Clojuric? Hmm.
; Anyways. Each thing is a 3-member, uh, triple.

; How does decomposing parameters work if the data structure doesn't actually match?
(defn tri-node [[val l-c r-c]]
  [val l-c r-c])
(tri-node '(:a :b :c))
(tri-node '(:a :b))
(tri-node '(:a))

; Hmm. That nil will be an issue. Let's see.
(defn tri-node-2 [[val l-c r-c :as node]]
  (if (= (count node) 3)
    true
    false))
(= false (tri-node-2 '(:a)))
(= true (tri-node-2 '(:a :b :c)))
(= false (tri-node-2 []))
; Okay, that's a good way to do it.

(defn test-btree [[val l-chld r-chld :as node]]
  (if (or (= (count node) 3) (not= val nil))
    (if (= nil l-chld r-chld)
    false
    
(fn test-btree [[val l-chld r-chld :as node]]
  (cond
    (= node nil) true
    (or (not (or (= nil l-chld) (coll? l-chld)))
        (not (or (= nil r-chld) (coll? r-chld)))
        (= val nil) 
        (not= (count node) 3)) 
      false
    :else (and (test-btree l-chld) (test-btree r-chld))))