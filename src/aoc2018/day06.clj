(ns aoc2018.day06
  (:use aoc2018.driver))

(def input-pattern #"(\d+), (\d+)")

(defn parse-point [line]
  (let [[_ x-str y-str] (re-matches input-pattern line)]
    {:x (Integer/valueOf x-str) :y (Integer/valueOf y-str)}))

(defn abs [^Integer x]
  (Math/abs x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn bounded-in-dir? [p q dir]
  (let [dx (- (:x q) (:x p))
        dy (- (:y q) (:y p))]
    (case dir
      :down  (and (pos? dy) (>= (abs dy) (abs dx)))
      :up    (and (neg? dy) (>= (abs dy) (abs dx)))
      :right (and (pos? dx) (>= (abs dx) (abs dy)))
      :left  (and (neg? dx) (>= (abs dx) (abs dy))))))

(defn bounded-in? [p points]
  (let [points* (remove p points)]
    (and (some #(bounded-in-dir? p % :up) points*)
         (some #(bounded-in-dir? p % :down) points*)
         (some #(bounded-in-dir? p % :left) points*)
         (some #(bounded-in-dir? p % :right) points*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsolution day06 [input]
  (let [points (->> input clojure.string/split-lines (map parse-point))
        bounded-points (filter #(bounded-in? % points) points)]
    [bounded-points
     0]))
