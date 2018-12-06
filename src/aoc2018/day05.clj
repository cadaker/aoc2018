(ns aoc2018.day05
  (:use aoc2018.driver))

(defn ucase [^Character u]
  (Character/toUpperCase u))

(defn lcase [^Character u]
  (Character/toLowerCase u))

(defn reaction? [u1 u2]
  (and (not= u1 u2)
       (= (ucase u1) (ucase u2))))

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
  (remove #{(ucase unit-to-remove) (lcase unit-to-remove)} units))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsolution day05 [input]
  (let [reacted (react (clojure.string/trim input))
        all-units (set (map ucase reacted))
        ;; min-key might apply its key func multiple times, so precompute the values
        ;; instead of running the expensive react calls multiple times.
        pruned-lengths (map (comp count react (partial remove-unit reacted)) all-units)]
    [(count reacted) (apply min pruned-lengths)]))
