(ns aoc2018.day25
  (:use aoc2018.driver)
  (:require [aoc2018.union-find :as union-find]))

(def pattern #"(-?\d+),(-?\d+),(-?\d+),(-?\d+)")

(defn value-of [^String s]
  (Integer/valueOf s))

(defn parse-line [line]
  (let [m (re-matches pattern line)]
    [(value-of (nth m 1)) (value-of (nth m 2)) (value-of (nth m 3)) (value-of (nth m 4))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn abs [^Integer x]
  (java.lang.Math/abs x))

(defn mh-dist [[x y z t] [x' y' z' t']]
  (+ (abs (- x x')) (abs (- y y')) (abs (- z z')) (abs (- t t'))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn join-point [uf p]
  (reduce (fn [uf p']
            (if (<= (mh-dist p p') 3)
              (union-find/join uf p p')
              uf))
          (union-find/make-set uf p)
          (keys uf)))

(defn process-points [points]
  (reduce join-point union-find/empty points))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn constellations [uf]
  (set (map #(second (union-find/find uf %)) (keys uf))))

(defn count-constellations [uf]
  (count (constellations uf)))

(defsolution day25 [input]
  (let [points (set (map parse-line (clojure.string/split-lines input)))]
    [(count-constellations (process-points points))
     0]))
