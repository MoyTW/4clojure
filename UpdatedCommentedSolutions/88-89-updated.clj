;;;; 88 - Symmetric Difference
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/88-wip.clj
;; Original:
(fn xor-set [lset rset]
  (cond
   (empty? lset) rset
   (empty? rset) lset
   :else
   (let [un (clojure.set/intersection lset rset)]
     (apply conj (apply disj lset un) (apply disj rset un)))))
;;   So, basically, the problem is to take two sets and return the xor of the
;; sets. Sounds trivial, right? Well, it is pretty easy.
;;   The code we have checks for empty left, checks for empty right, and if both
;; have members, it removes from both sets the intersection and then returns a
;; set constructed from the two mashed together. The algorithm, I think, is
;; fairly solid. It even handles the case of (xor-set #{} #{}) properly,
;; returning #{}.
;;   However, the code has a fatal error! If you def it and try (xor-set #{:a :b
;; :c} #{:a :c}), what will happen? What should happen is that it returns #{:b};
;; what actually happens is you get an arity error, because it's trying to
;; (apply conj ... #{}), passing in nothing as the second and more arguments to
;; conj! So, while the code does pass the tests on 4Clojure, it is fatally
;; flawed! So, let's fix that:
(fn xor-set [lset rset]
  (cond
    (empty? lset) rset
    (empty? rset) lset
    :else 
    (let [inter (clojure.set/intersection lset rset)]
      (clojure.set/union (apply disj lset inter) (apply disj rset inter)))))
;;   It's basically the same algorithm, just edited so it won't explode if you
;; pass in the wrong parameters. If you prefer to avoid that clumsy let/disj
;; thing that's going on there, you could use the following snippet code in the
;; else, instead:
(clojure.set/union (clojure.set/difference rset lset)
                   (clojure.set/difference lset rset))
;;   That's a little wordy, and it might be a tad more inefficient (depends on
;; the implementation of disj, set/difference; I don't know the details - and
;; it's a micro-optimization anyhow, which probably doesn't matter). It'd look
;; prettier if you used/imported the clojure.set functions.

;;;; 89 - Graph Tour
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/89-wip.clj
;; Original:
(fn assumes-connected-euler? [edge-vec]
  (letfn [(record-node [m n]
                       (let [f-n (first n)
                             l-n (last n)
                             with-f (assoc m f-n (inc (get m f-n 0)))]
                         (assoc with-f l-n (inc (get with-f l-n 0)))))
          (fold [m n]
                (let [f-n (first n) l-n (last n)]
                  (if (not= f-n l-n) (conj m f-n l-n) m)))
          (to-set [edges]
                  (apply conj #{} (flatten edges)))
          (connected? [edges]
                      (if (= (to-set edges) (reduce fold #{} edges))
                        true
                        false))]
    (if-not (connected? edge-vec) false
      (->> edge-vec
           (reduce record-node {})
           (filter #(odd? (last %)))
           (count)
           (>= 2)))))
;;   Okay, first off, the "Intent-to-leading-edge-of-input-parameters" style of
;; indenting is not my favorite, so let me redo the indentation. Also, what's
;; that (if true true false) doing? Let's cut that out:
(fn assumes-connected-euler? [edge-vec]
  (letfn [(record-node [m n]
            (let [f-n (first n)
                  l-n (last n)
                  with-f (assoc m f-n (inc (get m f-n 0)))]
              (assoc with-f l-n (inc (get with-f l-n 0)))))
          (fold [m n]
            (let [f-n (first n) l-n (last n)]
              (if (not= f-n l-n) (conj m f-n l-n) m)))
          (to-set [edges]
            (into #{} (flatten edges)))
          (connected? [edges]
            (= (to-set edges) (reduce fold #{} edges)))]
    (if-not (connected? edge-vec) false
      (->> edge-vec
           (reduce record-node {})
           (filter #(odd? (last %)))
           (count)
           (>= 2)))))
;;   There are actually two parts to this, here. First, we detect whether it's
;; connected (though, as we'll see later, what it's actually doing is less than
;; that) and, if it is connected, it searches for whether it's possible to take
;; a tour with every edge visited only once. Let's concentrate on the code to
;; detect if it's connected:
(fold [m n]
  (let [f-n (first n) l-n (last n)]
    (if (not= f-n l-n) (conj m f-n l-n) m)))
(to-set [edges]
  (into #{} (flatten edges)))
(connected? [edges]
  (= (to-set edges) (reduce fold #{} edges)))
;;   This is a particularly obtuse set of code, mainly due to the fact that I'm
;; using a reduce where I should use a filter! What (reduce fold #{} edges) is
;; doing is conj-ing into a set all elements which are not loops (so, it keeps
;; [:a :b] but not [:a :a]). The logic of this is that if there's a node only
;; accessible by a looped edge, then it won't be added into the set created by
;; the reduce, and therefore, this algorithm will detect any fully unconnected
;; nodes. If there are no nodes which are unconnected, it must be connected.
;; There's a flaw in the logic there but first, let's refactor the code, since
;; this is just terribly confusing.
;;   Like I said earlier, this is using a reduce when it really should be
;; utilizing a filter. For example, we can rewrite connected? like so:
(connected? [edges]
  (= (distinct (flatten edges))
     (distinct (flatten (filter #(not= (first %) (second %)) edges)))))
;;   Granted, that nesting is pretty fugly (and I use distinct just because I
;; always use set to the same effect), but it is significantly shorter, and
;; hopefully a little more clear. We can actually thread it, if we want:
(connected? [edges]
  (= (distinct (flatten edges))
     (->> (filter #(not= (first %) (second %)) edges)
          (flatten)
          (distinct))))
;;   I don't think in this case, it's a significant improvement. Anyhow, it's a
;; fundamentally flawed algorithm, because what if we pass in the following
;; input:
(connected? [[:a :b] [:c :d]])
;;   The graph formed by this input is something like:
;;     :a   :c
;;      |    |
;;     :b   :d
;;   which is clearly unconnected! However, connected? returns true!
;;   The reason is that the algorithm is only searching for fully unconnected
;; nodes, and since none of the nodes is fully unconnected - :a is connected to
;; :b and :d is connected to :c - it goes ahead and declares that the graph is
;; connected! This is good enough to pass the tests for the problem, but not
;; robust enough to actually detect whether the graph is connected!
;;   We can actually test whether the graph is connected with a relatively simple
;; BFS. To make it easier, we can transform the edges into a more traditional
;; map as follows:
(defn edges-to-map [edges]
  (reduce (fn [m [k v]] (assoc m k (conj (get m k []) v)))
          {}
          (mapcat (fn [[f s]] [[f s] [s f]])
                  edges)))
(def m (edges-to-map [[:a :b] [:c :d] [:a :d]]))
;; Given this more conventional map, we can conduct a BFS:
(defn bfs [graph nodes visited]
  (if (empty? nodes) visited
      (let [new-visited (apply conj (into #{} visited) nodes)
            new-nodes (remove new-visited (mapcat #(graph %) nodes))]  
        (recur graph new-nodes new-visited))))
(bfs m '(:c) [])

;;   So, we can tie it together into one connected function by Frakensteining it
;; like so:
(defn connected? [edges]
  (letfn [(bfs [graph nodes visited]
            (if (empty? nodes) visited
              (let [new-visited (apply conj (into #{} visited) nodes)
                    new-nodes (remove new-visited (mapcat #(graph %) nodes))]
                (recur graph new-nodes new-visited))))]
       (= (set (flatten edges))
          (bfs (->> (mapcat (fn [[f s]] [[f s] [s f]]) edges)
                    (reduce (fn [m [k v]] (assoc m k (conj (get m k []) v))) {}))
               [(ffirst edges)]
               []))))
;;   Properly, it's three functions - a bfs function, a edges->map function, and
;; a third function which compares them - but, again, 4Clojure is a harsh
;; mistress.

;;   The second half tries to ascertain, given that the graph is connected,
;; whether you can traverse all edges once. The algorithm here is we count the
;; edges each node has. If all nodes have an even number of edges, there's a
;; path (you can always exit the node you enter) - except that since we're doing
;; edges, you can have two one-degree nodes, since those can be your start and
;; end points.
;;   So, basically, the record-node function will be used to map the edges to a
;; frequency count, then you count how many one-degree nodes there are, and if
;; it's less than or equal to two you return true, else false.
(fn edge-tour? [edge-vec]
  (letfn [(record-node [m n]
                       (let [f-n (first n)
                             l-n (last n)
                             with-f (assoc m f-n (inc (get m f-n 0)))]
                         (assoc with-f l-n (inc (get with-f l-n 0)))))
          ...]
    (if-not (connected? edge-vec) false
      (->> edge-vec
           (reduce record-node {})
           (filter #(odd? (last %)))
           (count)
           (>= 2)))))
;;   Readers may observe that the record-node reduction is basically terribly
;; written, and could be replaced with the following line:
(frequencies (flatten edge-vec))
;;   That would give us:
(fn edge-tour? [edge-vec]
  (letfn [...]
    (if-not (connected? edge-vec) false
      (->> (frequencies (flatten edge-vec))
           (filter #(odd? (last %)))
           (count)
           (>= 2)))))
;;   There we go.

;;   Now, to put it all together:
(fn edge-tour? [edge-vec]
  (letfn [(connected? [edges]
            (letfn [(bfs [graph nodes visited]
                      (if (empty? nodes) visited
                        (let [new-visited (apply conj (into #{} visited) nodes)
                              new-nodes (remove new-visited (mapcat #(graph %) nodes))]
                          (recur graph new-nodes new-visited))))]
              (= (set (flatten edges))
                 (bfs (->> (mapcat (fn [[f s]] [[f s] [s f]]) edges)
                           (reduce (fn [m [k v]] (assoc m k (conj (get m k []) v))) {}))
                      [(ffirst edges)]
                      []))))]
    (if-not (connected? edge-vec) false
      (->> (frequencies (flatten edge-vec))
           (filter #(odd? (last %)))
           (count)
           (>= 2)))))
;;   This is in the area of seventeen lines, but honestly, a lot of them are
;; really squashed together, and it could probably be decompressed significantly
;; so it's acceptably readable on first look.
;;   Again, I have a compactness fetish, sooo...