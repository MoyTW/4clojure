;; so, check if they're disjoint
;; Also, apparently, one of the test cases is the function distinct? soooo...huh.

(for [left #{:a :b :c :d :e}
      right #{:a :b :c :d}
      :when (= left right)]
  :shared)

(defn disjoint? [left right]
  (zero? (count (for [l left
                      r right
                     :when (= l r)]
                  :shared))))
  
(def p4
  #{#{'(:x :y :z) '(:x :y) '(:z) '()}
    #{#{:x :y :z} #{:x :y} #{:z} #{}}
    #{'[:x :y :z] [:x :y] [:z] [] {}}})
  
(for [s p4
      :let [other (filter #(not= s %) p4)]]
  (for [r other
        :when (= s r)]
    [s r]))

(defn __ [sets]
  (empty?
    (apply concat
           (for [s sets
                 :let [other (filter #(not= s %) sets)]]
             (for [r other
                   :when (not (disjoint? s r))]
               :covers)))))
        
(def p2
  #{#{:a :b :c :d :e}
  #{:a :b :c :d}
  #{:a :b :c}
  #{:a :b}
  #{:a}})

(__ p2)

(__ #{#{:a :b} #{:c :d} #{:e :f} #{:g :h}})

(= (__ #{#{[1 2 3] [4 5]}
         #{[1 2] [3 4 5]}
         #{[1] [2] 3 4 5}
         #{1 2 [3 4] [5]}})
   true) ; true
(= (__ #{#{'a 'b}
         #{'c 'd 'e}
         #{'f 'g 'h 'i}
         #{''a ''c ''f}})
   true) ; true
(= (__ #{#{'(:x :y :z) '(:x :y) '(:z) '()}
         #{#{:x :y :z} #{:x :y} #{:z} #{}}
         #{'[:x :y :z] [:x :y] [:z] [] {}}})
   false) ; true
(= (__ #{#{(= "true") false}
         #{:yes :no}
         #{(class 1) 0}
         #{(symbol "true") 'false}
         #{(keyword "yes") ::no}
         #{(class '1) (int \0)}})
   false) ; true
(= (__ #{#{distinct?}
         #{#(-> %) #(-> %)}
         #{#(-> %) #(-> %) #(-> %)}
         #{#(-> %) #(-> %) #(-> %)}})
   true) ; true
(= (__ #{#{(#(-> *)) + (quote mapcat) #_ nil}
         #{'+ '* mapcat (comment mapcat)}
         #{(do) set contains? nil?}
         #{, , , #_, , empty?}})
   false) ; true
   
   
(defn disjoint? [left right]
  (zero? (count (for [l left
                      r right
                     :when (= l r)]
                  :shared))))
(fn __ [sets]
  (letfn [(disjoint? [left right]
            (zero? (count (for [l left
                                r right
                               :when (= l r)]
                            :shared))))]
    (empty?
      (apply concat
             (for [s sets
                   :let [other (filter #(not= s %) sets)]]
               (for [r other
                     :when (not (disjoint? s r))]
                 :covers))))))