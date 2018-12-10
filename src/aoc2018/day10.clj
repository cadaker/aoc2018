(ns aoc2018.day10
  (:use aoc2018.driver))

(def input-pattern #"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>")

(defn parse-input [line]
  (let [[_ px py vx vy] (re-matches input-pattern line)
        value-of #(Integer/valueOf %)]
    {:x (value-of px) :y (value-of py) :vx (value-of vx) :vy (value-of vy)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn closest-time [p1 p2]
  "Find the t where ||p1(t) - p2(t)|| is the smallest"
  (let [dx (- (:x p1) (:x p2))
        dy (- (:y p1) (:y p2))
        dvx (- (:vx p1) (:vx p2))
        dvy (- (:vy p1) (:vy p2))]
    (int (- (/ (+ (* dx dvx) (* dy dvy))
               (+ (* dvx dvx) (* dvy dvy)))))))

(defn neighbours? [p1 p2]
  (let [dx (- (:x p1) (:x p2))
        dy (- (:y p1) (:y p2))]
    (#{[0 1] [0 -1] [1 0] [-1 0]} [dx dy])))

(defn has-neighbour? [p ps]
  (some (partial neighbours? p) ps))

(defn many-neighbours? [ps]
  (let [neighboured (filter #(has-neighbour? % ps) ps)]
    (> (count neighboured) (* 3/4 (count ps)))))

(defn step-points [ps t]
  (for [p ps]
    {:x (+ (:x p) (* t (:vx p)))
     :y (+ (:y p) (* t (:vy p)))}))

(defn bbox [ps]
  (let [left (apply min (map :x ps))
        right (apply max (map :x ps))
        top (apply min (map :y ps))
        bottom (apply max (map :y ps))]
    {:left left :right right :top top :bottom bottom}))

(defn print-points [ps]
  (let [box (bbox ps)
        lookup (into #{} (map (fn [p]
                                [(:x p) (:y p)])
                              ps))]
    (doseq [y (range (:top box) (inc (:bottom box)))]
      (doseq [x (range (:left box) (inc (:right box)))]
        (if (lookup [x y])
          (print "#")
          (print " ")))
      (println))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsolution day10 [input]
  [(let [points (map parse-input (clojure.string/split-lines input))
         guess-t (closest-time (first points) (second points))]
     (doseq [t (range (- guess-t 100) (+ guess-t 100))]
       (let [ps (step-points points t)]
         (when (many-neighbours? ps)
           (print-points ps)
           (println t)))))
   nil])
