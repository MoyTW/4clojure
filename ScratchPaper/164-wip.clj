;; heeey DFAs
;; Enumerate all strings huh
;; wait how is that - uuuuh. So, "Return a lazy sequence of all strings" then, because DFAs can recognize infinitely long sequences (For example, DFA approving a sequence of all 1s - recognizes 1, 11, 111 ... infinity 1s).
;; That's, hmm.

;; Well, first, let's figure out how to do it without regards to laziness. How do you enumerate all strings?

;; Take the first:
;; S.0 -> (1)
;;         |
;;         V
;; (3) <- (2)

;; It's basically a BFS, except you store the edges instead of the hmmmmmmm. hmmmmmm is it that simple? Let's see. DFAs are digraphs which basically switch on/off depending on the ending node, soooo...yeah. It really is that simple. Also it should be a lazy sequence.

(defn test []
  (__ '{:states #{q0 q1 q2 q3 q4 q5 q6 q7}
              :alphabet #{e h i l o y}
              :start q0
              :accepts #{q2 q4 q7}
              :transitions {q0 {h q1}
                            q1 {i q2, e q3}
                            q3 {l q5, y q4}
                            q5 {l q6}
                            q6 {o q7}}}))
(defn memtest []
  (let [res (take 2000 (__ '{:states #{q0 q1}
                             :alphabet #{0 1}
                             :start q0
                             :accepts #{q0}
                             :transitions {q0 {0 q0, 1 q1}
                                           q1 {0 q1, 1 q0}}}))]
    (and (every? (partial re-matches #"0*(?:10*10*)*") res)
         (= res (distinct res)))))

;(defn step [nodes {:keys [accepts transitions] :as dfa}]
;  (let [accepted (map first (filter (comp accepts second) nodes))]
;    (map #(vector (first %) (transitions (second %))) nodes)))

(def dfa-state
  [["hi" 'q2] ["he" 'q3]])
  
;(defn __ [{:keys [states alphabet start accepts transitions] :as dfa}]
;  (step [["" start]] [] dfa))

(defn resolve-step [[string node] transitions]
  (let [outgoing (transitions node)]
    (if outgoing
        (map #(vector (str string (first %)) (second %)) outgoing))))

;(defn step [nodes {:keys [accepts transitions] :as dfa}]
;  (let [accepted (map first (filter (comp accepts second) nodes))
;        next-nodes (mapcat #(resolve-step % transitions) nodes)]
;    (do (prn [accepted next-nodes])
;    (if (seq next-nodes)
;        (concat accepted (step next-nodes dfa))
;        accepted))) )

;; Okay, that's a rough idea of how it should go. Not lazy, but it's a good starting point.
;; Also it should be non-stack consuming because you only need one state, really.
;; Let's work on those.

;; Making it lazy
(defn __ [{:keys [states alphabet start accepts transitions] :as dfa}]
  (step [["" start]] dfa))
  
(defn step [nodes {:keys [accepts transitions] :as dfa}]
  (let [accepted (map first (filter (comp accepts second) nodes))
        next-nodes (mapcat #(resolve-step % transitions) nodes)]
    (if (seq next-nodes)
        (concat accepted (lazy-seq (step next-nodes dfa)))
        accepted)))