;; Immediate, obvious solution: fill algorithm 
;; Also, Djikstra's algorithm  But I always think of Djikstra's algorithm, even when it might be mildly inappropriate to do so 

;; Okay, let's do a fill  First, we replace the M with an F, like so:
;; "#######"
;; "#     #"
;; "#  #  #"
;; "#F # C#"
;; "#######"
;; Then, each pass, we follow the following algorithm:
;;   For each F, replace all blank adjacent squares with an F
;;   Replace the square with the F with a #

;; "#######"
;; "#     #"
;; "#F #  #"
;; "##F# C#"
;; "#######"

;; "#######"
;; "#F    #"
;; "##F#  #"
;; "#### C#"
;; "#######"

;; "#######"
;; "##F   #"
;; "####  #"
;; "#### C#"
;; "#######"

;; and so on and so on 
;; You know you have reached the end state when one of the following occurs:
;;   No Fs remain - there is no path
;;   You find a C - there is a path

;; We actually should add an intermediate step, like so:
;; Each pass:
;;   For each F, replace all bank adjacent squares with an N
;;   Replace all Fs with #s
;;   Replace all Ns with Fs

(
(defn mark-next [coll]
  (rest (first (reduce (fn [[out p c] n]
                         (if (and (= c \space) (or (= p \F) (= n \F))) 
                             [(conj out \N) \N n]
                             [(conj out c) c n]))
                       [[] nil nil]
                       (conj (into [] coll) \#)))))
"#F  #")
;; Hmm let's see if using ->> makes it nicer
(
(defn mark-next [coll]
  (->> (conj (into [] coll) \#)
       (reduce (fn [[out p c] n]
                 (if (and (= c \space) (or (= p \F) (= n \F)))
                     [(conj out \N) \N n]
                     [(conj out c) c n]))
                [[] nil nil])
        ((comp rest first))))
"#F  #")
;; Uh, no, not really  I guess it might be clearer?
;; Oh, and, we also overwrite Cs 
(
(defn mark-next [coll]
  (->> (conj coll \#)
       (reduce (fn [[out p c] n]
                 (if (and (contains? #{\space \C} c) (or (= p \F) (= n \F)))
                     [(conj out \N) \N n]
                     [(conj out c) c n]))
                [[] nil nil])
        ((comp rest first))))
"#F  #")


(
(defn swap-chars [coll]
  (map (fn [c]
         (cond
           (= c \N) \F
           (= c \F) \#
           :else c))
       coll))
[\# \F \N \# \N \F \N \#])

(
(defn all-next [coll]
  (->> (into [] coll)
       (map mark-next)
       (apply map vector)
       (map mark-next)
       (map swap-chars)
       (apply map vector)))
["#######"
 "#     #"
 "#  #  #"
 "#F # C#"
 "#######"])
 
(
(defn for-science [coll]
  (loop [maze (map #(into [] (clojure.string/replace % #"M" "F")) coll)]
    (let [flat-maze (flatten maze)]
      (cond
        (not-any? #{\C} flat-maze) true
        (not-any? #{\F} flat-maze) false
        :else (recur (all-next maze))))))
["#######"
 "#     #"
 "#  #  #"
 "#M # C#"
 "#######"])
 
(for-science
[
"########"
"#M  #  #"
"#   #  #"
"# # #  #"
"#   #  #"
"#  #   #"
"#  # # #"
"#  #   #"
"#  #  C#"
"########"])