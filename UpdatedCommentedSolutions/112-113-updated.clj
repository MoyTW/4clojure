;;;; 112 - Sequs Horriblis
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/112-wip.clj
;; Original:
(fn what [n coll]
  (letfn [(process-next [cnt in]
            (loop [cnt cnt out [] in in]
              (let [head (first in)]
                (cond
                  (= head nil) out
                  (coll? head) (conj out (process-next cnt head))
                  (> (+ cnt head) n) out
                  :else (recur (+ cnt head) (conj out head) (rest in))))))]
    (process-next 0 coll)))
;;   This problem is interesting because you need to proceed in two directions -
;; left and downwards. The first thing to note here is that the unit tests all
;; feature strictly ascending sequences of integers, so we assume that this will
;; always be the case. Also note that the incoming sequence may be infinite!
;;   If you wanted to be super sure you could add in something like:
;;     {:pre [(and (>= n 0)
;;                 (= (flatten (take n coll))
;;                    (sort < (flatten (take n coll)))))]}
;;   Anyways! Assuming that the above is true, we basically need to run an if
;; that looks something like:
;;     (if (next element is an integer)
;;         (recurse to the right)
;;         (recurse downwards))
;;   That's basically what the above code is doing, except a little confusingly
;; because of inconsistent formatting. While it looks very intimidating, it's
;; actually quite simple! One thing that I've noticed I did a lot that adds some
;; really unnecessary bloat is using a loop/recur structure when a fn/recur
;; structure would actually work just fine. Also, the conditions are kinda
;; janky. Let's just reformat it quickly:
(fn what [n coll]
  (letfn [(process-next [cnt out in]
            (let [head (first in)]
              (cond
                (or (= head nil) (> (+ cnt head) n)) ; End condition
                  out
                (coll? head) ; Recurses downwards
                  (conj out (process-next cnt [] head))
                :else ; Recurses sideways
                  (recur (+ cnt head) (conj out head) (rest in)))))]
    (process-next 0 [] coll)))
;;   There's kind of an awkward thing happening here, when it uses recur when it
;; goes sideways but when it goes downwards it uses traditional, stack-consuming
;; recursion. We could actually refactor it to use only recur, but we would then
;; also need to have some sort of "conj-to-deepest-element" function to drill
;; down and modify the data structure anyways.
;;   One of the things I've noticed about these is that as I've gotten further
;; and further along, the number of times where I look at my past solutions and
;; go "Well, this is clearly wrong! I could do it better with this other
;; algorithm/other method/standard function!" has gone down.
;;   Also I still have no good idea of how to work with deeply modified data
;; structures, even now; most of the Clojure standard functions seem to work
;; more horizontally than vertically. Deep nesting stuff still tends to go
;; straight back to traditional recursion. I wonder if I'm missing something
;; very important...?

;;;; 113 - Making Data Dance
 ;;; Scratch:;;   https://github.com/MoyTW/4clojure/blob/master/ScratchPaper/113-wip.clj
;; Original:
(fn [& args]
  (reify
    java.lang.Object
    (toString [_]
      (clojure.string/join ", " (sort args)))
    clojure.lang.ISeq
      (seq [_]
        (if (empty? args) nil
          (distinct args)))))
;;   Oh, this one! This one was interesting, because it's basically dealing with
;; Java Interop! First, you have to figure out what exactly the str and seq
;; functions are doing. The easiest way is to go look at the documentation
;; and/or source.
;;   str is really easy, actually; from the documentation: "With one arg x,
;; returns x.toString()" So, we basically need to encapsulate the parameters in
;; a Java object whose toString() inserts commas.
;;   seq is weirder. When you call seq it actually goes and defers it to
;; clojure.lang.RT's implementation of seq, which is actually written in Java!
;;   So, what we basically need to do is override those particular
;; implementations, which you can fairly easily do with reify. Apparently reify
;; is preferred over Proxy anyways, unless the specific task at hand isn't able
;; to be resolved with reify, so...
;;   I'd guess that the idea of this one is basically to get you to look up proxy
;; and learn about Java interop, and how to overwrite Java classes and use
;; reify/proxy, and if it was it worked on me.