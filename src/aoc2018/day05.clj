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

(defsolution day05 [input]
  [(count (react (clojure.string/trim input)))
   0])
