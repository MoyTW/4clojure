;; sooooo
(fn __ [initial & args]
  (let [ops (partition 2 args)]
    (reduce #((first %2) %1 (second %2)) initial ops)))