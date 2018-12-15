(ns aoc2018.day15
  (:use aoc2018.driver))

(def HP 200)

(defn read-map-line [[cave units] [row line]]
  (reduce (fn [[cave units] col]
            (case (get line col)
              \G [(assoc cave [row col] \.) (assoc units [row col] {:type :goblin :hp HP})]
              \E [(assoc cave [row col] \.) (assoc units [row col] {:type :elf :hp HP})]
              \. [(assoc cave [row col] \.) units]
              \# [(assoc cave [row col] \#) units]))
          [cave units]
          (range (count line))))

(defn read-map [input]
  (reduce read-map-line [{} {}] (map-indexed vector (clojure.string/split-lines input))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn valid? [cave [row col]]
  (and (= \. (cave [row col]))))

(defn in-range? [cave units pos]
  (and (valid? cave pos) (nil? (units pos))))

(defn all-neighbours [cave [row col]]
  "Return the valid (non-wall) neighbour coordinates of the point"
  (filter (partial valid? cave) (list [(dec row) col] [row (dec col)] [row (inc col)] [(inc row) col])))

(defn neighbours [cave units pos]
  "Return the in-range neighbour coordinates of the point, in reading order."
  (filter (partial in-range? cave units) (all-neighbours cave pos)))

(defn target-positions [cave units unit-type [row col]]
  "Return a set of all the positions where a unit of type unit-type at [row col] would want to go."
  (let [target-unit-positions (for [e units :when (not= (:type (val e)) unit-type)]
                                (key e))]
    (set (mapcat (partial neighbours cave units) target-unit-positions))))

(defn find-move [cave units unit-type [row col]]
  (let [target? (target-positions cave units unit-type [row col])
        start-steps (neighbours cave units [row col])]
    (loop [queue (reduce conj clojure.lang.PersistentQueue/EMPTY start-steps)
           initial-direction (into {} (for [rc start-steps] [rc rc]))
           visited? #{[row col]}]
      (if (empty? queue)
        nil
        (let [[r c] (peek queue)
              queue' (pop queue)]
          (cond (target? [r c])
                (initial-direction [r c])

                (visited? [r c])
                (recur queue' initial-direction visited?)

                :else
                (let [new-steps (neighbours cave units [r c])]
                  (recur (reduce conj queue' new-steps)
                         (into initial-direction (for [s new-steps
                                                       :when (nil? (initial-direction s))]
                                                   [s (initial-direction [r c])]))
                         (conj visited? [r c])))))))))

(defn neighbour-target [cave units unit-type pos]
  (let [target? (fn [rc]
                  (and (units rc)
                       (not= unit-type (:type (units rc)))))]
    (first (filter target? (all-neighbours cave pos)))))

(defn move-unit-to [units start-pos end-pos]
  (assoc (dissoc units start-pos) end-pos (units start-pos)))

(defn do-round [cave units]
  (loop [actions (sort-by key units)
         units units]
    (if (empty? actions)
      units
      (let [[pos {unit-type :type}] (first actions)
            close-target (neighbour-target cave units unit-type pos)
            new-pos (find-move cave units unit-type pos)]
        (cond close-target
              ;; TODO: attack
              (recur (rest actions) units)
              ;; Move to a new spot
              new-pos
              (recur (rest actions) (move-unit-to units pos new-pos))
              ;; No reachable target
              :else
              (recur (rest actions) units))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsolution day15 [input]
  [0 0])
