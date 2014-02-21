;; Does clojure have a generic sort?
;; Apparently it does. We can use this.

(defn __ [coll]
  (distinct (sort coll)))
(__ [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
(def sorted (__ [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11]))

;; Now how do we get it to recognize groupings? Well, in order to do that we need to look forward/behind.
;; So, a 1-shift, or a reduce.

(defn r [[out head prev] in]
  (cond
    (= (inc prev) in) [out head in]
    :else [(conj out [head prev]) in in]))
(reduce r [[] (first sorted) (first sorted)] (rest sorted))
(let [[d fl sl] (reduce r [[] (first sorted) (first sorted)] (rest sorted))]
  (conj d [fl sl]))

(fn __ [coll]
  (let [sorted (distinct (sort coll))
        red (fn [[out head prev] in]
              (cond
                (= (inc prev) in) [out head in]
                :else [(conj out [head prev]) in in]))
        [out last-first last-last] 
          (reduce red [[] (first sorted) (first sorted)] (rest sorted))]
    (conj out [last-first last-last])))
;; oh fails on empty
;; yeah, it would, with this setup
(fn __ [coll]
  (if (seq coll)
    (let [sorted (distinct (sort coll))
          red (fn [[out head prev] in]
                (cond
                  (= (inc prev) in) [out head in]
                  :else [(conj out [head prev]) in in]))
          [out last-first last-last] 
            (reduce red [[] (first sorted) (first sorted)] (rest sorted))]
      (conj out [last-first last-last]))
    coll))