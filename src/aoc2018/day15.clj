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

(defn find-round-move [cave units unit-type pos]
  (let [close-target (neighbour-target cave units unit-type pos)
        pos' (find-move cave units unit-type pos)]
    (cond close-target pos
          pos' pos'
          :else pos)))

(defn attack-target [units target-pos attack-power]
  (if (> (:hp (units target-pos)) attack-power)
    [(update-in units [target-pos :hp] #(- % attack-power)) [-1000 -1000]]
    [(dissoc units target-pos) target-pos]))

(defn factions-remaining [units]
  (set (map :type (vals units))))

(defn remove-unit-action [actions pos]
  (remove #(= pos (first %)) actions))

(defn do-round-with-stopped-flag [cave units attack-powers]
  (loop [actions (sort-by key units)
         units units]
    (cond (empty? actions) ; Are we done?
          [units false]

          (= 1 (count (factions-remaining units)))
          [units true]

          :else
          (let [[pos {unit-type :type}] (first actions)
                pos' (find-round-move cave units unit-type pos)
                units' (move-unit-to units pos pos')
                target (neighbour-target cave units' unit-type pos')]
            (if target
              (let [[units'' killed] (attack-target units' target (attack-powers unit-type))]
                (recur (remove-unit-action (rest actions) killed) units''))
              (recur (rest actions) units'))))))

(def default-attack-powers {:goblin 3 :elf 3})

(defn do-round
  ([cave units attack-powers]
     (first (do-round-with-stopped-flag cave units attack-powers)))
  ([cave units]
     (do-round cave units default-attack-powers)))

(defn fight [cave units attack-powers]
  (loop [round-no 0
         units units
         stopped false]
    (if (= 1 (count (factions-remaining units)))
      [round-no units stopped]
      (let [[units' stopped'] (do-round-with-stopped-flag cave units attack-powers)]
        (recur (inc round-no) units' stopped')))))

(defn combat-outcome [[round-no units stopped]]
  (let [total-hp (reduce + (map :hp (vals units)))
        round-fudge (if stopped -1 0)]
    (* (+ round-no round-fudge) total-hp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn monotonic-find-first [func]
  (let [upper-bound
        (loop [n 1]
          (if (func n)
            n
            (recur (* n 2))))]
    (loop [lower 0
           upper upper-bound]
      (if (= lower upper)
        lower
        (let [mid (quot (+ upper lower) 2)]
          (if (func mid)
            (recur lower mid)
            (recur (inc mid) upper)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn count-elves [units]
  (count (filter #{:elf} (map :type (vals units)))))

(defn all-elves-survive? [cave units attack-power]
  (let [[_ units' _] (fight cave units {:goblin 3 :elf attack-power})]
    (= (count-elves units) (count-elves units'))))

(defsolution day15 [input]
  (let [[cave units] (read-map input)]
    [(combat-outcome (fight cave units default-attack-powers))
     (let [attack-power (monotonic-find-first (partial all-elves-survive? cave units))]
       (combat-outcome (fight cave units {:goblin 3 :elf attack-power})))]))
