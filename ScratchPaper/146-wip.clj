;; So use for here I guess
(defn __ [m]
  (for [kv m
        v kv]
    [kv v]))
(__ '{a {p 1, q 2} b {m 3, n 4}})

(defn __ [m]
  (for [kv m
        v (second kv)]
    [(first kv) v]))
(__ '{a {p 1, q 2} b {m 3, n 4}})

(defn __ [m]
  (for [kv m
        v (second kv)]
    {[(first kv) (first v)] (second v)}))
(__ '{a {p 1, q 2} b {m 3, n 4}})

(defn __ [m]
  (apply merge
         (for [kv m
               v (second kv)]
           {[(first kv) (first v)] (second v)})))
(__ '{a {p 1, q 2} b {m 3, n 4}})
;; hooo that's ugly can we like, destructure this
;; oh wait :let

(defn __ [m]
  (apply merge
         (for [kv m
               v (second kv)
               :let [key (first kv)
                     [fv rv] v]]
           {[key fv] rv})))
(__ '{a {p 1, q 2} b {m 3, n 4}})
;; yeah that didn't help