(ns aoc2018.day05
  (:use aoc2018.driver))

(defn reaction? [u1 u2]
  (and (not= u1 u2)
       (= (Character/toUpperCase u1) (Character/toUpperCase u2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn react [units]
  (clojure.string/join
   (reverse
    (reduce (fn [stack unit]
              (if (and (seq stack)
                       (reaction? (first stack) unit))
                (rest stack)
                (cons unit stack)))
            ()
            units))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn remove-unit [units unit-to-remove]
  (remove #{(Character/toUpperCase unit-to-remove)
            (Character/toLowerCase unit-to-remove)} units))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsolution day05 [input]
  (let [reacted (react (clojure.string/trim input))
        all-units (set (map #(Character/toUpperCase %) reacted))
        ;; min-key might apply its key func multiple times, so precompute the values
        ;; instead of running the expensive function multiple times.
        pruned (into {} (map (fn [unit]
                               [unit (count (react (remove-unit reacted unit)))])
                             all-units))
        [_ best-length] (apply min-key val pruned)]
    [(count reacted) best-length]))
