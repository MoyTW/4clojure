(
(fn []
  (let [x "(fn [] (let [x "
        y "(str x \" x \" "y" \" y \" y)) ]"]
    (str x \" x \" "y" \" y \" y)))
)

;; The x/y thing messes it up
(
(fn []
  (let [x ["(fn [] (let [x "
           "] (str (first x) x (last x))))"]]
    (str (first x) x (last x))))
)

(fn [] (let [x ["(fn [] (let [x" "] (str (first x) x (last x)"]] (str (first x) x (last x))))
(fn [] (let [x["(fn [] (let [x" "] (str (first x) x (last x)"]] (str (first x) x (last x)


(fn [] (let [x ["(fn [] (let [x " "] (str (first x) x (last x)"]] (str (first x) x (last x))))
(fn [] (let [x ["(fn [] (let [x " "] (str (first x) x (last x)"]] (str (first x) x (last x)

(fn [] (let [x ["(fn [] (let [x " "] (str (first x) x (last x))"]] (str (first x) x (last x))))
(fn [] (let [x ["(fn [] (let [x " "] (str (first x) x (last x))"]] (str (first x) x (last x))

(fn [] (let [x ["(fn [] (let [x " "] (str (first x) x (last x))))"]] (str (first x) x (last x))))
(fn [] (let [x ["(fn [] (let [x " "] (str (first x) x (last x))))"]] (str (first x) x (last x))))

(fn [] (let [x ["(fn [] (let [x " "] (str (first x) x (last x))))"]] (str (first x) x (last x))))