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
                       (not= unit-type (:type (units rc)))))
        all-targets (filter target? (all-neighbours cave pos))
        lowest-hp (apply min HP (map #(:hp (units %)) all-targets))]
    (first (filter #(= lowest-hp (:hp (units %))) all-targets))))

(defn move-unit-to [units start-pos end-pos]
  (assoc (dissoc units start-pos) end-pos (units start-pos)))

(def attack-power 3)

(defn find-round-move [cave units unit-type pos]
  (let [close-target (neighbour-target cave units unit-type pos)
        pos' (find-move cave units unit-type pos)]
    (cond close-target pos
          pos' pos'
          :else pos)))

(defn attack-target [units target-pos]
  (if (> (:hp (units target-pos)) attack-power)
    (update-in units [target-pos :hp] #(- % attack-power))
    (dissoc units target-pos)))

(defn do-round [cave units]
  (loop [actions (sort-by key units)
         units units]
    (cond (empty? actions) ; Are we done?
          units
          (nil? (units (ffirst actions))) ; Have we been killed
          (recur (rest actions) units)
          :else
          (let [[pos {unit-type :type}] (first actions)
                pos' (find-round-move cave units unit-type pos)
                units' (move-unit-to units pos pos')
                target (neighbour-target cave units' unit-type pos')]
            (if target
              (recur (rest actions) (attack-target units' target))
              (recur (rest actions) units'))))))

(defn factions-remaining [units]
  (set (map :type (vals units))))

(defn fight [cave units]
  (loop [round-no 0
         units units]
    (if (= 1 (count (factions-remaining units)))
      [round-no units]
      (recur (inc round-no) (do-round cave units)))))

(defn combat-outcome [[round-no units]]
  (let [total-hp (reduce + (map :hp (vals units)))]
    (* round-no total-hp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsolution day15 [input]
  (let [[cave units] (read-map input)]
    [(combat-outcome (fight cave units))
      0]))
