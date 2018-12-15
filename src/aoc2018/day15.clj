(ns aoc2018.day15
  (:use aoc2018.driver))

(defn read-map-line [[cave units] [row line]]
  (reduce (fn [[cave units] col]
            (case (get line col)
              \G [(assoc cave [row col] \.) (assoc units [row col] {:type :goblin :hp 200})]
              \E [(assoc cave [row col] \.) (assoc units [row col] {:type :elf :hp 200})]
              \. [(assoc cave [row col] \.) units]
              \# [(assoc cave [row col] \#) units]))
          [cave units]
          (range (count line))))

(defn read-map [input]
  (reduce read-map-line [{} {}] (map-indexed vector (clojure.string/split-lines input))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn in-range? [cave units [row col]]
  (and (= \. (cave [row col])) (nil? (units [row col]))))

(defn neighbours [cave units [row col]]
  "Return the in-range neighbour coordinates of the point, in reading order."
  (filter (partial in-range? cave units) (list [(dec row) col] [row (dec col)] [row (inc col)] [(inc row) col])))

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
                         (into initial-direction (for [s new-steps]
                                                   [s (initial-direction [r c])]))
                         (conj visited? [r c])))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsolution day15 [input]
  [0 0])
