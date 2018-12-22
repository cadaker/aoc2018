(ns aoc2018.day22
  (:use aoc2018.driver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compute-geologic-index [target-xy erosion-fn xy]
  (cond
   (= xy {:x 0 :y 0}) 0
   (= xy target-xy) 0
   (zero? (:y xy)) (* (:x xy) 16807)
   (zero? (:x xy)) (* (:y xy) 48271)
   :else (* (erosion-fn (update xy :x dec))
            (erosion-fn (update xy :y dec)))))

(defn compute-erosion [depth target-xy erosion-fn xy]
  (mod (+ depth
          (compute-geologic-index target-xy erosion-fn xy))
       20183))

(defn compute-erosions [depth target-xy maxx maxy]
  (reduce (fn [erosions xy]
            (assoc erosions xy (compute-erosion depth target-xy erosions xy)))
          {}
          (for [x (range (inc maxx)) y (range (inc maxy))]
            {:x x :y y})))

(defn cave-type [erosion]
  (case (mod erosion 3)
    0 :rocky
    1 :wet
    2 :narrow))

(def risk-level {:rocky 0 :wet 1 :narrow 2})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def input-depth 5913)
(def input-target-xy {:x 8 :y 701})

(defn total-risk [erosions]
  (reduce + (map (comp risk-level cave-type) (vals erosions))))

(defsolution day22 [_]
  (let [erosions (compute-erosions input-depth
                                   input-target-xy
                                   (:x input-target-xy)
                                   (:y input-target-xy))]
    [(total-risk erosions)
     0]))
