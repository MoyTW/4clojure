; Oh boy, Game of Life. I remember writing this in High School. We used Turtle Graphics or Logo or whatever it's called, and I made an old-school dungeon-crawler map, complete with a minimap, in my off-time.
; I think I had a sort of buffer-swap thing going on. It'd do all the calculations into a new grid (so you didn't have to calculate in-place) and then copy that whole thing over the old one.
; Anyways! This is going to get kind of weird for me, since I'd normally do this by iterating through a 2-D array...
; We can't do that here, though...well I mean it's technically possible, but, uh. Yeah, no. Another way.

; So, I was at a total loss as to how to do this in a way that didn't involve arrays, so I did a quick Google search for game life clojure, and skimmed one of the results. I took care not to read too carefully, because that would defeat the whole point of doing this exercise, but the upshot of it is that you can reformulate the way you think about the problem so that instead of tracking the arrays, you track the living cells as coordinate pairs.

; So, let's do that!

; So, we could represent
; 5 "      "  
; 4 " ##   "
; 3 " ##   "
; 2 "   ## "
; 1 "   ## "
; 0 "      "
;    012345
; as
; #{[1 3] [1 4] [2 3] [2 4] [3 1] [3 2] [4 1] [4 2]}

; So, to see if a cell lives or dies, we simply search the set for the eight adjacent squares. If the set contains 2 or more, it lives. Wait, hold on, let me start that over, that's not entirely correct.

; To see if a cell is alive, we do the following:
; living = (count living adjacent to)
;   If cell is live:
;       If (< living 2), set to dead
;       If (> living 3), set to dead
;       Else, set to living
;   If cell is dead:
;       If (= living 3), set to living

; We repeat this procedure for each cell on the "board", within the defined x, y bounds of the board. So, basically, we take in the height and width, generate a list of coordinates from [0...x] [0...y], reduce on list to get living cells.

; Also, we need a conversion from array of string rows to set of living cells, and vice versa. Leave for last.

; How to count adjacent cells?
; Get a set of the 8 adjacents
; Reduce over set of 8 adjacents, incrementing result if next in set of living cells

; Function to generate 8 adjacent coordinates from a given pair
(defn adj-coords [coords]
  (let [x (first coords) y (second coords)]
    (into #{}
      (for [x-n [(dec x) x (inc x)]
            y-n [(dec y) y (inc y)]
            :when (not= [x-n y-n] coords)]
        [x-n y-n]))))
      
(= (adj-coords [0 0]) #{[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 1] [1 0] [1 -1]})
(= (adj-coords [2 2]) #{[1 1] [1 2] [1 3] [2 1] [2 3] [3 1] [3 2] [3 3]})

; Function to count adjacent living cells
(defn count-adj [coords cells]
  (let [adj (adj-coords coords)]
    (reduce #(if (contains? cells %2) (inc %1) %1) 0 adj)))

(= 0 (count-adj [0 0] #{[1 3] [1 4] [2 3] [2 4] [3 1] [3 2] [4 1] [4 2]}))
(= 2 (count-adj [2 1] #{[1 3] [1 4] [2 3] [2 4] [3 1] [3 2] [4 1] [4 2]}))
(= 4 (count-adj [3 2] #{[1 3] [1 4] [2 3] [2 4] [3 1] [3 2] [4 1] [4 2]}))
(= 0 (count-adj [-10 -21] #{[1 3] [1 4] [2 3] [2 4] [3 1] [3 2] [4 1] [4 2]}))

; Function to decide if cell should live or die
(defn lives? [coords cells]
  (let [num-live (count-adj coords cells)
        is-live (contains? cells coords)]
    (if is-live
      (cond
        (< num-live 2) false
        (> num-live 3) false
        :else true)
      (if (= num-live 3)
        true
        false))))

(= false (lives? [2 2] #{[1 3] [1 4] [2 3] [2 4] [3 1] [3 2] [4 1] [4 2]}))
(= false (lives? [3 2] #{[1 3] [1 4] [2 3] [2 4] [3 1] [3 2] [4 1] [4 2]}))
(= true (lives? [4 1] #{[1 3] [1 4] [2 3] [2 4] [3 1] [3 2] [4 1] [4 2]}))

; Okay, on to the resolution
; Generate list of coordinates from [0...x] [0...y] - for gens lazy (though it doesn't matter anyways)
(defn gen-coords [x-max y-max]
  (for [x-n (take x-max (range))
        y-n (take y-max (range))]
    [x-n y-n]))
    
; Given height, width, and a set of cells, generate next step
(defn step [cells height width]
  (let [to-check (gen-coords width height)]
; Wait, actually, we can use gen-coords with lives? to do what we want.
(defn step [cells x-max y-max]
  (for [x-n (take x-max (range))
        y-n (take y-max (range))
        :when (lives? [x-n y-n] cells)]
    [x-n y-n]))
    
(= #{[1 3] [1 4] [2 4] [3 1] [4 1] [4 2]} (into #{} (step #{[1 3] [1 4] [2 3] [2 4] [3 1] [3 2] [4 1] [4 2]} 5 5)))

; Function to collapse vector of strings to set
; First, function to collapse single string to set, given a y-coordinate and maximum number of x
(
(defn collapse-str [s y max-x]
  (into #{} 
    (filter     
      #(not= % nil) 
      (map #(if (= \# %2) [%1 y] nil) (take max-x (range)) s))))
"#   #" 1 5)
; Uh, how about threading it? It's kind of a mess this way.
(
(defn collapse-str [s y]
  (->> s
       (map #(if (= \# %2) [%1 y] nil) (take (count s) (range)))
       (filter #(not= % nil))
       (into #{})))
"     " 1)
; Eh, still kinda a mess. It's the if in the anon func that does it.
; Oh, and it returns an empty set if nothing found.

(
(defn collapse-vec [v]
  (->> v
       (map #(vector %1 %2) (reverse (take (count v) (range))))
       (reduce #(concat %1 (collapse-str (last %2) (first %2))) [])
       (into #{})))
["      " " ##   " " ##   " "   ## " "   ## " "      "])

(= (collapse-vec ["      " " ##   " " ##   " "   ## " "   ## " "      "]) #{[1 3] [1 4] [2 3] [2 4] [3 1] [3 2] [4 1] [4 2]})

(
(defn explode-str [s max-x]
  (apply str (reduce #(assoc %1 %2 \#) (into [] (take max-x (repeat \space))) s)))
#{1 2 4} 5)

; Whoa, ugly.  
(
(defn explode-set [s max-x max-y]
  (let [live-map (reduce #(assoc %1 (last %2) (conj (get %1 (last %2) []) (first %2)))
                         (zipmap (take max-y (range)) (take max-y (repeat [])))
                         s)
        explode-str (fn [s-e]
                      (apply str (reduce #(assoc %1 %2 \#) 
                                         (into [] (take max-x (repeat \space))) 
                                         s-e)))]
    (reverse (map #(explode-str (get live-map %))
      (into [] (take max-y (range)))))))
#{[0 0] [1 0] [2 0] [3 0] [4 0] [5 0] [1 3] [1 4] [2 3] [2 4] [3 1] [3 2] [4 1] [4 2]} 6 6)

; Okay, uh, that's just unbearably ugly, let's see if we can't shine it up a bit
(defn explode-set [s max-x max-y]
  (let [spaces-list (into [] (take max-x (repeat \space)))
        y-range (take max-y (range))
        live-map (reduce 
                  (fn map-set-to-rows [mp st]
                    (assoc mp 
                      (last st)
                      (conj (get mp (last st)) (first st))))
                  (zipmap y-range (take max-y (repeat [])))
                  s)
        explode-str (fn [s-e]
                      (->> s-e
                           (reduce #(assoc %1 %2 \#) spaces-list)
                           (apply str)))]
    (->> y-range
         (into [])
         (map #(explode-str (get live-map %)))
         (reverse))))
; It turns out I have no idea how to make Clojure code more readable...

; ...I can't help but feel that there's a *much* better approach. Oh well, we're almost there. Let's collapse the (step) function and all its associated defs into one function.
(defn adj-coords [coords]
  (let [x (first coords) y (second coords)]
    (into #{}
      (for [x-n [(dec x) x (inc x)]
            y-n [(dec y) y (inc y)]
            :when (not= [x-n y-n] coords)]
        [x-n y-n]))))
(defn count-adj [coords cells]
  (let [adj (adj-coords coords)]
    (reduce #(if (contains? cells %2) (inc %1) %1) 0 adj)))
(defn lives? [coords cells]
  (let [num-live (count-adj coords cells)
        is-live (contains? cells coords)]
    (if is-live
      (cond
        (< num-live 2) false
        (> num-live 3) false
        :else true)
      (if (= num-live 3)
        true
        false))))
(defn step [cells x-max y-max]
  (for [x-n (take x-max (range))
        y-n (take y-max (range))
        :when (lives? [x-n y-n] cells)]
    [x-n y-n]))
; ...too big, at least for 4Clojure. Once the input/output form is added it'll be huge and confusing and terrible. Wouldn't be bad if you could use defs, but, eh. Okay. Let's think this one through.
; We can do away with adj-coords returning a set, we don't need that (it's a set anyways!)
; Also, we're decomposing coords in a let. We could do that in the parameter.
(defn adj-coords [[x y]]
    (for [x-n [(dec x) x (inc x)]
          y-n [(dec y) y (inc y)]
          :when (not= [x-n y-n] [x y])]
      [x-n y-n]))
; Okay, count-adj is...sure, that works.
; lives? is too long. Let's see. Oh, hey. We could fold the conditions together a bit by using a set:
(defn lives? [coords cells]
  (let [num-live (count-adj coords cells)]
    (if (contains? cells coords)
      (boolean (#{2 3} num-live))
      (boolean (#{3} num-live)))))