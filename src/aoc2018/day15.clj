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

(defn target-positions [cave units unit-type]
  "Return a set of all the positions where a unit of type unit-type would want to go."
  (let [target-unit-positions (for [e units :when (not= (:type (val e)) unit-type)]
                                (key e))]
    (set (mapcat (partial neighbours cave units) target-unit-positions))))

(defn bfs [cave units start-pos state update-state]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start-pos)
         visited? #{}
         state state]
    (if (empty? queue)
      state
      (let [pos (peek queue)
            queue' (pop queue)]
        (if (visited? pos)
          (recur queue' visited? state)
          (let [steps (neighbours cave units pos)]
            (recur (reduce conj queue' steps)
                   (conj visited? pos)
                   (update-state state pos steps))))))))

(defn distance-map [cave units pos]
  (let [update-distances (fn [distances pos steps]
                           (reduce (fn [distances step]
                                     (let [dist (inc (distances pos))]
                                       (if (nil? (distances step))
                                         (assoc distances step dist)
                                         (update distances step #(min % dist)))))
                                   distances
                                   steps))]
    (bfs cave units pos {pos 0} update-distances)))

(defn find-best-target [cave units unit-type pos]
  (let [distances (distance-map cave units pos)
        target? (target-positions cave units unit-type)
        reachable-targets (sort (filter target? (keys distances)))
        min-distance (if (seq reachable-targets)
                       (apply min (map distances reachable-targets))
                       nil)]
    (first (filter #(= (distances %) min-distance) reachable-targets))))

(defn find-move [cave units unit-type pos]
  (let [target (find-best-target cave units unit-type pos)
        initial-directions (bfs cave units pos
                                {}
                                (fn [initial-direction pos steps]
                                  (let [dir (initial-direction pos)]
                                    (into initial-direction (for [s steps
                                                                  :when (nil? (initial-direction s))]
                                                              [s (or dir s)])))))]
    (initial-directions target)))

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
    [(update-in units [target-pos :hp] #(- % attack-power)) [-1000 -1000]]
    [(dissoc units target-pos) target-pos]))

(defn factions-remaining [units]
  (set (map :type (vals units))))

(defn remove-unit-action [actions pos]
  (remove #(= pos (first %)) actions))

(defn do-round-with-stopped-flag [cave units]
  (loop [actions (sort-by key units)
         units units
         stopped false]
    (cond (empty? actions) ; Are we done?
          [units stopped]

          :else
          (let [stopped' (= 1 (count (factions-remaining units)))
                [pos {unit-type :type}] (first actions)
                pos' (find-round-move cave units unit-type pos)
                units' (move-unit-to units pos pos')
                target (neighbour-target cave units' unit-type pos')]
            (if target
              (let [[units'' killed] (attack-target units' target)]
                (recur (remove-unit-action (rest actions) killed) units'' stopped'))
              (recur (rest actions) units' stopped'))))))

(defn do-round [cave units]
  (first (do-round-with-stopped-flag cave units)))

(defn fight [cave units]
  (loop [round-no 0
         units units
         stopped false]
    (if (= 1 (count (factions-remaining units)))
      [round-no units stopped]
      (let [[units' stopped'] (do-round-with-stopped-flag cave units)]
        (recur (inc round-no) units' stopped')))))

(defn combat-outcome [[round-no units stopped]]
  (let [total-hp (reduce + (map :hp (vals units)))
        round-fudge (if stopped -1 0)]
    (* (+ round-no round-fudge) total-hp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsolution day15 [input]
  (let [[cave units] (read-map input)]
    [(combat-outcome (fight cave units))
      0]))
