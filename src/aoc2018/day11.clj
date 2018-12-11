(ns aoc2018.day11
  (:use aoc2018.driver))

(defn abs [n]
  (if (> n 0)
    n
    (- n)))

(defn hundred-digit [n]
  (abs (rem (quot n 100) 10)))

(defn power-level [serial [x y]]
  (let [rack-id (+ x 10)
        power-1 (+ (* y rack-id) serial)
        power-2 (* power-1 rack-id)
        power-3 (hundred-digit power-2)]
    (- power-3 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn grid-around [w h [x y]]
  (for [x' (range w), y' (range h)]
    [(+ x x') (+ y y')]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def serial-number 7989)

(def width 300)
(def height 300)

(defn power-of-grid [serial w h xy]
  (reduce + (map (partial power-level serial)
                 (grid-around w h xy))))

(defsolution day11 [_]
  [(let [xys (for [x (range 1 (- width 1)), y (range 1 (- height 1))]
               [x y])]
     (apply max-key (partial power-of-grid serial-number 3 3) xys))
   0])
