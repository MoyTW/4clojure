; Problems 34 to 37, new and shiny with commentary!

; 34 - Implement range
; Original:
(fn custom-range [begin end]
  (loop [begin begin, end end, range-list []]
    (if (= begin end)
      range-list
      (recur (inc begin) end (conj range-list begin)))))
;   So what's going on here is that it's loop/recur-ing up from the beginning
; value until it reaches the end value, incrementing and conj-ing the value as
; it steps forwards. One thing that jumps out is I passed end in through the
; loop/recur statement, which is totally unnecessary.
;   How's the algorithm? Well...let's see, it runs once for each value between
; begin and end, which is a fair enough efficiency. Can you pass in any valid
; values that break it? Well, if you pass in a begin that's lower than end, it 
; goes for infinity, so that's hardly ideal. So we could put a :pre statement
; in there to check that the values aren't all wrong. We could allow equal
; begins and ends (returning a blank) or disallow them. I'll allow them here.

; Cleanup:
(fn custom-range [begin end]
  {:pre [(<= begin end)]}
  (loop [begin begin, range-list []]
    (if (= begin end) range-list
      (recur (inc begin) (conj range-list begin)))))

;   We could also do this with iterate. Easy enough, just start at begin and 
; take the difference, as so:
(fn custom-range [begin end]
  {:pre [(<= begin end)]}
  (take (- end begin) (iterate inc begin)))
; I like it!

; 35 - Local bindings
; Original:
7 7 7
;   This is one of those "Introducing a new concept!" ones. Here, we've got 
; let. Let is basically read-only assignment. Well, it's not exactly that, but 
; it's close enough. So, let's move on!

; 36 - Let it Be
; Original:
[x 7, y 3, z 1]
; Hurmm, more introduction to let. We'll pass this one without comment.

; 37 - Regular Expressions
; Original:
"ABC"
;   Introduction to regular expressions! They're basically Java regexes, and 
; you declare them with #"". So, go look up the documentation if you need it.

;   Well this was disappointingly short! There was only one real interesting 
; one in here! That's...unfortunate. I'll put 38-45 up too, because this was so
; sadly cut short.