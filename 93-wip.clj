; Flatten, but leave one level intact, huh? Well.
; Okay, well, what did my flatten code look like?
(fn c-flatten [coll] 
  (let [l (first coll) r (next coll)]
    (concat 
     (if (sequential? l) (c-flatten l) [l])
     (if (sequential? r) (c-flatten r)))))

; Allrighty. The first thought that comes to mind is "Modify the old flatten code, but have it look one more level downwards!"
; so let's do something silly
(fn c-flatten [coll] 
  (let [l (first coll) r (next coll)]
    (concat 
     (if (sequential? (first l)) (c-flatten l) [l])
     (if (sequential? (first r)) (c-flatten r)))))
; Hey guess what? It worked. Well, that was easy.