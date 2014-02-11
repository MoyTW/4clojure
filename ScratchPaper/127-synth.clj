(fn jesus-wept [bitmap]
  (letfn [(int-to-array [i c]
            (->> (Integer/parseInt i)
                 (format (str \% \0 c \d))
                 (map #(= \1 %))
                 (into [])))
          (ints-to-array [bitmap]
            (let [strs (map #(Integer/toBinaryString %) bitmap)
                  pad-to (apply max (map count strs))]
              (into [] (map #(int-to-array % pad-to) strs))))
          (next-coords [coords]
            (let [c (ffirst coords)
                  r-min (last (first coords))
                  r-max (last (last coords))]
              (for [r (range (dec r-min) (+ 2 r-max))]
                [(dec c) r])))
          (largest-vertical [coords mine n]
            (if (every? true? (map #(get-in mine %) coords))
                (recur (next-coords coords) mine (+ n (count coords)))
                n))
          (next-ne [coords]
            (into #{} (mapcat (fn [[r c]] [[(dec r) c] [r (inc c)]]) coords)))
          (largest-diagonal [coords mine n]
            (if (every? true? (map #(get-in mine %) coords))
                (recur (next-ne coords) mine (+ n (count coords)))
                n))
          (gen-mine-permutations [mine]
            [mine
             (into [] (map (comp vec reverse) (apply map vector mine)))
             (into [] (reverse (map (comp vec reverse) mine)))
             (into [] (reverse (apply map vector mine)))])
          (find-max [coord mine]
            (max (largest-diagonal [coord] mine 0)
                 (largest-vertical [coord] mine 0)))]
    (let [mine (ints-to-array bitmap)
          all-coords (for [r (range (count mine)) c (range (count (first mine)))] [r c])
          all-mines (gen-mine-permutations mine)
          area (apply max (for [c all-coords m all-mines] (find-max c m)))]
      (if (> area 2) area))))