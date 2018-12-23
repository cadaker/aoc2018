(ns aoc2018.day23
  (:use aoc2018.driver))

(def pattern #"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)")

(defn value-of [^String s]
  (Integer/valueOf s))

(defn parse-line [line]
  (let [m (re-matches pattern line)
        get-value (fn [i] (value-of (nth m i)))]
    [[(get-value 1) (get-value 2) (get-value 3)] (get-value 4)]))

(defn parse-input [input]
  (into #{} (map parse-line (clojure.string/split-lines input))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn abs [^Integer x]
  (java.lang.Math/abs x))

(defn mh [[x0 y0 z0] [x1 y1 z1]]
  (+ (abs (- x0 x1)) (abs (- y0 y1)) (abs (- z0 z1))))

(defn biggest-region [regions]
  (apply max-key second regions))

(defn regions-inside [regions [pos radius]]
  (for [[p _] regions
        :when (<= (mh pos p) radius)]
    p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsolution day23 [input]
  (let [regions (parse-input input)]
    [(let [biggest (biggest-region regions)]
       (count (regions-inside regions biggest)))
     0]))
