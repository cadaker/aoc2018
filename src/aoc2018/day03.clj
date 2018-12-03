(ns aoc2018.day03
  (:use aoc2018.driver))

(defn get-rect [input-match]
  (let [entry #(Integer/valueOf (nth input-match %))]
    {:x (entry 2)
     :y (entry 3)
     :w (entry 4)
     :h (entry 5)}))

(defn paint-rect [grid rect]
  (let [rect-coords (for [x (range (:x rect) (+ (:x rect) (:w rect)))
                          y (range (:y rect) (+ (:y rect) (:h rect)))]
                      [x y])]
    (reduce (fn [grid' coord]
              (update grid' coord #(inc (or % 0))))
            grid
            rect-coords)))

(defn paint-grid [rects]
  (reduce paint-rect {} rects))

(defn count-grid [grid]
  (count (filter #(> % 1) (vals grid))))

(def INPUT-PATTERN #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)\n")

(defsolution day03 [input]
  (let [entries (re-seq INPUT-PATTERN input)
        rects (map get-rect entries)
        grid (paint-grid rects)]
    [(count-grid grid)
     0]))
