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

(defn overlaps [[pos0 r0] [pos1 r1]]
  (<= (mh pos0 pos1) (+ r0 r1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn overlap-graph [regions]
  (reduce (fn [graph [r0 r1]]
            (update graph r0 #(conj (or % #{}) r1)))
          {}
          (for [r0 regions, r1 regions
                :when (overlaps r0 r1)]
            [r0 r1])))

(defn constraint [region]
  (let [[[x y z] r] region]
    (+ x y z (- r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def clique-size 980) ;; Found by inspection of neighbour counts.

(defn get-big-subgraph [graph neighbour-limit]
  (apply dissoc graph (filter (fn [node] (< (count (graph node)) neighbour-limit)) (keys graph))))

(defn complete-intersection [graph]
  (apply clojure.set/intersection (vals graph)))

(defsolution day23 [input]
  (let [regions (parse-input input)]
    [(let [biggest (biggest-region regions)]
       (count (regions-inside regions biggest)))
     (let [total-graph (overlap-graph regions)
           graph (get-big-subgraph total-graph clique-size)
           clique (complete-intersection graph)]
       (apply max (map constraint clique)))]))
