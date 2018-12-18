(ns aoc2018.day18
  (:use aoc2018.driver))

(defn make-lookup [input]
  (let [lines (clojure.string/split-lines input)
        indexed (for [[row line] (map-indexed vector lines)
                      [col ch] (map-indexed vector line)]
                  [[row col] ch])]
    (into {} indexed)))

(defn print-grid [grid]
  (doseq [row (sort (set (map first (keys grid))))]
    (doseq [col (sort (set (map second (keys grid))))]
      (print (grid [row col])))
    (println)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn neighbours [grid [row col]]
  (filter grid (list [(dec row) (dec col)] [(dec row) col] [(dec row) (inc col)]
                     [row (dec col)] [row (inc col)]
                     [(inc row) (dec col)] [(inc row) col] [(inc row) (inc col)])))

(defn point-evolution [grid pos]
  (let [neighbour-elems (map grid (neighbours grid pos))]
    (case (grid pos)
      \. (if (>= (count (filter #{\|} neighbour-elems)) 3)
           \| \.)
      \| (if (>= (count (filter #{\#} neighbour-elems)) 3)
           \# \|)
      \# (if (and (>= (count (filter #{\#} neighbour-elems)) 1)
                  (>= (count (filter #{\|} neighbour-elems)) 1))
           \# \.))))

(defn evolve [grid]
  (into {} (for [pos (keys grid)]
             [pos (point-evolution grid pos)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn resource-value [grid]
  (* (count (filter #{\|} (vals grid)))
     (count (filter #{\#} (vals grid)))))

(defsolution day18 [input]
  (let [grid (make-lookup input)
        grids (iterate evolve grid)]
    [(resource-value (nth grids 10))
     0]))
