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

(defn bounds [points]
  {:left (apply min (map :x points))
   :right (apply max (map :x points))
   :bottom (apply max (map :y points))
   :top (apply min (map :y points))})

(defn manhattan-distance [p q]
  (+ (abs (- (:x p) (:x q)))
     (abs (- (:y p) (:y q)))))

(defn unique-closest-point [p points]
  (let [points* (remove p points)
        points-with-dist (map (fn [p*] [p* (manhattan-distance p p*)]) points*)
        [closest-point closest-dist] (apply min-key second points-with-dist)
        closest-count (count (filter #{closest-dist} (map second points-with-dist)))]
    (if (= 1 closest-count)
      closest-point
      nil)))

(defn find-areas [all-points bounded-points]
  (let [bounded-points-set (set bounded-points)
        box (bounds all-points)
        box-points (for [x (range (:left box) (inc (:right box)))
                         y (range (:top box) (inc (:bottom box)))]
                     {:x x :y y})]
    (reduce (fn [areas xy]
              (let [p (unique-closest-point xy all-points)]
                (if (and p (contains? bounded-points-set p))
                  (update areas p #(inc (or % 0)))
                  areas)))
            {}
            box-points)))

(defn within-distance? [p points max-distance]
  (let [distances (map #(manhattan-distance % p) points)]
    (< (reduce + distances) max-distance)))

;; Hmm, can we really assume that all the relevant points will be inside the bbox?
(defn find-area-of-within-region [all-points max-distance]
  (let [box (bounds all-points)
        candidate-points (for [x (range (:left box) (inc (:right box)))
                               y (range (:top box) (inc (:bottom box)))
                               :when (within-distance? {:x x :y y} all-points max-distance)]
                           {:x x :y y})]
    (count candidate-points)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def max-distance 10000)

(defsolution day06 [input]
  (let [points (->> input clojure.string/split-lines (map parse-point))
        bounded-points (filter #(bounded-in? % points) points)
        areas (find-areas points bounded-points)]
    [(apply max (vals areas))
     (find-area-of-within-region points max-distance)]))
