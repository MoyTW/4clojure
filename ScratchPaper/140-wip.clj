;; There are a lot of words whose meanings I don't know in that problem statement.
;; Oh. Oh! It's boolean logic. I was never amazing at boolean logic. I do have vague memories of the subject, though.
;; I think I had to learn about K-maps, and that class I got a B in? I didn't do so hot. Plus it seems all the memory has fallen out of my head!

;; So basically we need to get from here (using example 1):
;; (!a  B  C !d) +
;; ( A !b !c !d) +
;; ( A !b !c  D) +
;; ...
;; ( A  B  C !d)
;; to
;; (A + !c) + (A + !b) + (B C !d)

;; How do we do this thing?
;; Well, there are a bunch of rules you could apply to simplify it, I think.

;; hmm
;; http://sce.umkc.edu/~hieberm/281_new/lectures/simplification/simplification.html
;; http://en.wikipedia.org/wiki/Quine%E2%80%93McCluskey_algorithm
;; I don't want to mess around with trying to apply rules programatically
;; I haven't the slightest idea how to do a K-map in code. Hmm. Making it wouldn't be excessively hard, but how would I get it to process properly? As in, how would I get it to recognize where it should put its 'circles'?