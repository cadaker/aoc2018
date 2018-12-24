(ns aoc2018.day24
  (:use aoc2018.driver))

(def main-pattern #"(\d+) units each with (\d+) hit points.*with an attack that does (\d+) (\w+) damage at initiative (\d+)")

(def weak-pattern #"weak to ([^;\)]+)")
(def immune-pattern #"immune to ([^;\)]+)")

(defn value-of [^String s]
  (Integer/valueOf s))

(defn parse-unit [line]
  (let [main-m (re-matches main-pattern line)
        weak-m (re-find weak-pattern line)
        immune-m (re-find immune-pattern line)]
    (when main-m
      {:count (value-of (nth main-m 1))
       :hp (value-of (nth main-m 2))
       :attack (value-of (nth main-m 3))
       :attack-type (nth main-m 4)
       :initiative (value-of (nth main-m 5))
       :weak-to (when weak-m
                  (clojure.string/split (nth weak-m 1) #", "))
       :immune-to (when immune-m
                    (clojure.string/split (nth immune-m 1) #", "))})))

(defn parse-input [input]
  (let [[_ _ imm inf]
        (reduce (fn [[cur-faction n imm inf] line]
                  (cond
                   (= line "") [cur-faction n imm inf]
                   (= line "Immune System:") [:imm 0 imm inf]
                   (= line "Infection:") [:inf 0 imm inf]
                   (= cur-faction :imm) [cur-faction
                                         (inc n)
                                         (assoc imm n (parse-unit line))
                                         inf]
                   (= cur-faction :inf) [cur-faction
                                         (inc n)
                                         imm
                                         (assoc inf n (parse-unit line))]))
                [nil 0 {} {}]
                (clojure.string/split-lines input))]
    [imm inf]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn effective-power [unit]
  (* (:count unit) (* (:attack unit))))

(defn attack-order [units]
  (reverse (sort-by (fn [ix] [(effective-power (units ix)) (:initiative (units ix))]) (keys units))))

(defn damage [attacker defender]
  (let [power (effective-power attacker)]
    (cond
     (seq (filter #{(:attack-type attacker)} (:immune-to defender))) 0
     (seq (filter #{(:attack-type attacker)} (:weak-to defender))) (* power 2)
     :else power)))

(defn target-order [attacker defenders]
  (reverse (sort-by (fn [ix]
                      (let [defender (defenders ix)]
                        [(damage attacker defender) (effective-power defender) (:initiative defender)]))
                    (keys defenders))))

(defn target-selection [attackers defenders]
  (reduce (fn [targets attacker-id]
            (let [attacker (attackers attacker-id)
                  choices (remove (set (vals targets)) (target-order attacker defenders))]
              (assoc targets attacker-id (first choices))))
          {}
          (attack-order attackers)))

(defn remove-unit [lookup [faction id]]
  (update lookup faction (fn [x] (dissoc x id))))

(defn damage-unit [unit damage]
  (let [units-lost (quot damage (:hp unit))]
    (update unit :count (fn [c] (- c units-lost)))))

(defn combat-result [all-units unit-id damage]
  (let [unit (get-in all-units unit-id)]
    (if (>= damage (* (:hp unit) (:count unit)))
      (remove-unit all-units unit-id)
      (assoc-in all-units unit-id (damage-unit unit damage)))))

(defn with-faction [faction ids]
  (map (partial vector faction) ids))

(defn combat-action-order [all-units]
  (let [all-ids (concat (with-faction :imm (keys (:imm all-units)))
                        (with-faction :inf (keys (:inf all-units))))]
    (reverse (sort-by (fn [id] (:initiative (get-in all-units id))) all-ids))))

(defn combat-round [imm inf]
  (let [all-units {:imm imm :inf inf}
        all-targets {:imm (into {} (for [[k v] (target-selection imm inf)]
                                     [k [:inf v]]))
                     :inf (into {} (for [[k v] (target-selection inf imm)]
                                     [k [:imm v]]))}]
    (loop [action-order (combat-action-order all-units)
           remaining-units all-units]
      (let [attacker-id (first action-order)
            attacker (when attacker-id (get-in remaining-units attacker-id))
            defender-id (when attacker-id (get-in all-targets attacker-id))
            defender (when defender-id (get-in remaining-units defender-id))]
        (cond
         (and attacker defender)
         (let [dmg (damage attacker defender)]
           (recur (rest action-order) (combat-result remaining-units defender-id dmg)))

         (nil? attacker-id)
         [(:imm remaining-units) (:inf remaining-units)]

         :else
         (recur (rest action-order) remaining-units))))))

(defn fight [imm inf]
  (first (drop-while (fn [[imm inf]] (and (seq imm) (seq inf)))
                     (iterate (fn [[imm inf]] (combat-round imm inf)) [imm inf]))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn total-count [units]
  (reduce + (map :count (vals units))))

(defsolution day24 [input]
  (let [[imm-0 inf-0] (parse-input input)
        [final-imm final-inf] (fight imm-0 inf-0)]
    [(total-count (if (empty? final-imm) final-inf final-imm))
     0]))
