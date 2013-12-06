; We can check if something is a keyword using (keyword?)

(partition-by keyword? [:a 1 2 3 :b :c 4])
(mapcat  #(if (keyword? (first %)) % [%]) (partition-by keyword? [:a 1 2 3 :b :c 4]))

; We can reduce on this
(first (reduce
  (fn [[mp lst] nxt]
    (cond
      (and (keyword? lst) (keyword? nxt)) [(assoc mp lst []) nxt]
      (keyword? lst) [(assoc mp lst nxt) nxt]
      :else [mp nxt]))
  [{} nil]
  (mapcat  #(if (keyword? (first %)) % [%]) (partition-by keyword? [:a 1 2 3 :b :c 4]))))
; Kinda hack-y...

(fn build-map [kvs]
  (let [red-func
          (fn [[mp lst] nxt]
            (cond
              (and (keyword? lst) (keyword? nxt)) [(assoc mp lst []) nxt]
              (keyword? lst) [(assoc mp lst nxt) nxt]
              :else [mp nxt]))
        grouped-vals
          (mapcat #(if (keyword? (first %)) % [%])
                  (partition-by keyword? kvs))]
    (first (reduce red-func [{} nil] grouped-vals))))
; ugly but hey it works