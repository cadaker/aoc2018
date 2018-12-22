(ns aoc2018.day22
  (:use aoc2018.driver)
  (:require aoc2018.dijkstra))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- basic-geologic-index [target-xy xy]
  (cond
   (= xy {:x 0 :y 0}) 0
   (= xy target-xy) 0
   (zero? (:y xy)) (* (:x xy) 16807)
   (zero? (:x xy)) (* (:y xy) 48271)
   :else nil))

(defn compute-geologic-index [target-xy erosion-fn xy]
  (if-let [basic (basic-geologic-index target-xy xy)]
    basic
    (* (erosion-fn (update xy :x dec))
       (erosion-fn (update xy :y dec)))))

(defn- do-compute-erosion [depth geo-index]
  (mod (+ depth geo-index) 20183))

(defn compute-erosion [depth target-xy erosion-fn xy]
  (do-compute-erosion depth (compute-geologic-index target-xy erosion-fn xy)))

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

(defn erosion-at [depth target-xy erosion-cache xy]
  (if-let [cache-value (erosion-cache xy)]
    [erosion-cache (erosion-cache xy)]
    (if-let [b-index (basic-geologic-index target-xy xy)]
      (let [erosion (do-compute-erosion depth b-index)]
        [(assoc erosion-cache xy erosion) erosion])
      (let [x'y (update xy :x dec)
            xy' (update xy :y dec)
            [cache' _]  (erosion-at depth target-xy erosion-cache x'y)
            [cache'' _] (erosion-at depth target-xy cache' xy')
            erosion (compute-erosion depth target-xy cache'' xy)]
        [(assoc cache'' xy erosion) erosion]))))

(defn movable? [equip type]
  "True if we can move to a square of the given type, if we hold this type of equipment."
  (case type
    :rocky (#{:gear :torch} equip)
    :wet (#{:gear :neither} equip)
    :narrow (#{:torch :neither} equip)))

(defn valid-coord? [xy]
  (and (>= (:x xy) 0)
       (>= (:y xy) 0)))

(def all-equipment #{:torch :gear :neither})

(defn valid-steps [depth target-xy erosion-cache [equip xy]]
  "Returns a pair of first a new erosion-cache, second a list of [t [equip' xy']] for valid steps"
  (let [xys (filter valid-coord? (list (update xy :x inc) (update xy :x dec)
                                       (update xy :y inc) (update xy :y dec)))
        erosion-cache' (reduce (fn [cache xy]
                                 (first (erosion-at depth target-xy cache xy)))
                               erosion-cache
                               xys)
        equips (remove #{equip} all-equipment)]
    [erosion-cache'
     (concat (for [xy' xys
                   :let [erosion (second (erosion-at depth target-xy erosion-cache' xy'))]
                   :when (movable? equip (cave-type erosion))]
               [1 [equip xy']])
             (for [equip' equips]
               [7 [equip' xy]]))]))

(defn dijkstra [depth target-xy]
  (aoc2018.dijkstra/do-dijkstra
   [:torch [0 0]]
   [:torch [(:x target-xy) (:y target-xy)]]
   (fn [erosion-cache [equip [x y]]]
     (let [[erosion-cache' steps] (valid-steps
                                   depth
                                   target-xy
                                   erosion-cache
                                   [equip {:x x :y y}])]
       [erosion-cache' (for [[t [equip xy]] steps]
                         [t [equip [(:x xy) (:y xy)]]])]))
   {}))

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
     (dijkstra input-depth input-target-xy)]))
