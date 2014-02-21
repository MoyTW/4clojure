(fn __ [{:keys [states alphabet start accepts transitions] :as dfa}]
  (letfn [(resolve-step [[string node] transitions]
            (if-let [outgoing (transitions node)]
              (map #(vector (str string (first %)) (second %)) outgoing)))
          (step [nodes {:keys [accepts transitions] :as dfa}]
            (let [accepted (map first (filter (comp accepts second) nodes))
                  next-nodes (mapcat #(resolve-step % transitions) nodes)]
              (if (seq next-nodes)
                  (concat accepted (lazy-seq (step next-nodes dfa)))
                  accepted)))]
    (step [["" start]] dfa)))