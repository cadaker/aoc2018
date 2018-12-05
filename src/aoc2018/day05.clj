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
        best-unit (apply min-key (fn [unit]
                                   (count (react (remove-unit reacted unit))))
                         all-units)]
    [(count reacted)
     (count (react (remove-unit reacted best-unit)))]))
