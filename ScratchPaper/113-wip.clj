;; I have no idea how to possibly do this.
;; So. From my understanding, this program requires you to write something that will switch on which statement is called on it.
;; Now, if we were doing something in, say, C#, that would be pretty simple. We would implement an interface, or override ToString() or ToSequence() or something. But, uh, this is Clojure.
;; Well, apparently proxy does what we need, so I may as well go ahead and look up the documentation on that...

;; So, apparently, proxy is a Java interop function. It basically creates a proxy class implementing an interface, with the actual functions bound to the ones you pass in. Then, when the proxied object is called with those functions, it basically does what I was saying up to with the C#.

;; ...so, from this we can decude that Clojure's (str) actually goes out and hunts down to Java ToString() method. And, presumably, (seq) goes down into Java as well?
;; Confirmed; I checked the source of (str) and it does go down into ToString(). (seq) goes to clojure.lang.RT, so...that's written in Java.
;; So, this is basically "Intro To Java Object Interop" I suppose? I'm down, let's get this trainwreck on the road!

;; Also, also, I just learned that 2-semicolon comments should be aligned to the same level of indentation as the code. So, uh. There's no code so these are all left-side, but if you are commenting in code...three semicolons goes to the left margin always. The more ya know, and all that!

;; Anyways, I think I will use reify, if I can figure out how exactly to do so. The documentation is...pretty daunting, but here goes.
;;   Syntax is effectively:
;;      (reify spec*)
;;      <spec> ::= <interface> (<method> [args+] <body>)*
;; So what it basically is is "Reify this protocol/interface/object, defining it with the following zero or more methods." (Though - why it's a * and not a + confuses me...what's a "Reify Nothing At All" do?)
;; It...seems pretty simple, actually. Let's give it a shot.

;; So, we need to reify two functions. clojure.lang.RT's seq and Java's Object.toString.
(str (
(fn [& args]
  (reify
    java.lang.Object
    (toString [_]
      "zoom zoom")))
2 1 3))
;; Okay. Got it. So, to do the actual problem:
(str (
(fn [& args]
  (reify
    java.lang.Object
    (toString [_]
      (clojure.string/join ", " (sort args)))))
2 1 3))
;; I do believe I'm getting the hang of this. Now, we need to override seq.
(seq (
(fn [& args]
  (reify
    clojure.lang.RT
      (seq [_]
        (sort args))))
2 1 3))
;; Okay, the online REPL just screamed at me for touching clojure.lang.RT. Okay, I will admit, it is not ever a great idea to actually overwrite core functionality. So...how would I do this by interfaces or something?
(seq (
(fn [& args]
  (reify
    clojure.lang.ISeq
      (seq [_]
        (sort args))))
2 1 3))
;; whoop whoop
(seq (
(fn [& args]
  (reify
    clojure.lang.ISeq
      (seq [_]
        (sort args))))
2 1 3))

;; Aaaand
(str (
(fn [& args]
  (reify
    java.lang.Object
    (toString [_]
      (clojure.string/join ", " (sort args)))
    clojure.lang.ISeq
      (seq [_]
        (distinct args))))
1 2 4 5 3))
;; Oh, it wants me to return nil if passed in nothing (for the seq). Sure.
(fn [& args]
  (reify
    java.lang.Object
    (toString [_]
      (clojure.string/join ", " (sort args)))
    clojure.lang.ISeq
      (seq [_]
        (if (empty? args) nil
          (distinct args)))))
;; That was fun!