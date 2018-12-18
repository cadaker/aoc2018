(ns aoc2018.day17
  (:use aoc2018.driver))

(def pattern #"(.)=(\d+), (.)=(\d+)\.\.(\d+)")

(defn value-of [^String s]
  (Integer/valueOf s))

(defn parse-entry [points entry]
  (let [[_ c1 coord-str c2 start-str end-str] (re-matches pattern entry)
        coord (value-of coord-str)
        start (value-of start-str)
        end (value-of end-str)]
    (reduce conj points
            (map (if (= "y" c1)
                   #(vector coord %)
                   #(vector % coord))
                 (range start (inc end))))))

(defn parse-input [input]
  (reduce parse-entry #{} (clojure.string/split-lines input)))

(defn bounds-of [ys xs]
  [(apply min ys)
   (apply max ys)
   (apply min xs)
   (apply max xs)])

(defn make-grid [points]
  (let [[miny maxy minx maxx] (bounds-of (map first points) (map second points))]
    (into {} (for [x (range (dec minx) (+ 2 maxx)) y (range miny (inc maxy))]
               [[y x] (if (points [y x]) \# \.)]))))

(defn render-grid [grid]
  (let [[miny maxy minx maxx] (bounds-of (map first (keys grid)) (map second (keys grid)))
        render-line (fn [y]
                      (clojure.string/join (map grid (for [x (range minx (inc maxx))] [y x]))))]
    (clojure.string/join "\n" (map render-line (range miny (inc maxy))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def starting-x 500)

(defn paint-point [ch grid pos]
  (assoc grid pos ch))

(defn paint-points [ch grid points]
  (reduce (partial paint-point ch) grid points))

(defn starting-point [grid]
  [(apply min (map first (keys grid))) starting-x])

(defn scan-while [pred [dy dx] [start-y start-x]]
  (let [line (iterate (fn [[y x]] [(+ y dy) (+ x dx)]) [start-y start-x])]
    (first (drop-while pred line))))

(defn find-bottom [grid pos]
  (first (scan-while (fn [yx] (and (grid yx) (#{\. \|} (grid yx)))) [1 0] pos)))

(defn extend-water [grid pos dx]
  (let [[stop-y stop-x] (scan-while (fn [[y x]]
                                      (and (grid [y x])
                                           (not (#{\. \|} (grid [(inc y) x])))
                                           (not= \# (grid [y x]))))
                                    [0 dx]
                                    pos)
        points (for [x (range (second pos) stop-x dx)]
                 [(first pos) x])]
    (if (#{\. \|} (grid [(inc stop-y) stop-x]))
      [points (list [stop-y stop-x])]
      [points ()])))

(defn pour-water [grid pos]
  (let [bottom (find-bottom grid pos)]
    (paint-points \| grid (for [y (range (first pos) bottom)]
                            [y (second pos)]))))

(defn spread-water [grid pos]
  "Spread water sideways from the given point. Returns grid and a list of spill over points."
  (let [[points-right spills-right] (extend-water grid pos 1)
        [points-left spills-left] (extend-water grid pos -1)
        spills (concat spills-left spills-right)]
    [(paint-points (if (empty? spills) \~ \|) grid (concat points-left points-right))
     spills]))

(defn paint-water [grid [start-y start-x]]
  (let [bottom (find-bottom grid [start-y start-x])
        pour-grid (pour-water grid [start-y start-x])]
    (if (nil? (pour-grid [bottom start-x]))
      [pour-grid ()]
      (loop [spread-grid pour-grid spread-y (dec bottom)]
        (let [[spread-grid' spills] (spread-water spread-grid [spread-y start-x])]
          (if (or (seq spills) (nil? (grid [(dec spread-y) start-x])))
            [spread-grid' spills]
            (recur spread-grid' (dec spread-y))))))))

(defn paint-all-water [grid spills]
  (if (empty? spills)
    grid
    (let [[grid' spills'] (paint-water grid (first spills))]
      (recur grid' (set (concat (rest spills) spills'))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sum-water [grid]
  (count (filter #{\~ \|} (vals grid))))

(defn sum-retained-water [grid]
  (count (filter #{\~} (vals grid))))

(defsolution day17 [input]
  (let [points (parse-input input)
        grid (make-grid points)
        painted-grid (paint-all-water grid [(starting-point grid)])]
    [(sum-water painted-grid)
     (sum-retained-water painted-grid)]))
