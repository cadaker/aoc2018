(ns aoc2018.core
  (:use aoc2018.driver)
  (:gen-class))

(defn usage []
  (println "Arguments: <solution-no>")
  (println "  where solution-no is day01 etc."))

;; FIXME: remove
(defsolution "day01" []
  [0 0])

(defn -main
  "Main runner for the AoC solutions"
  [& args]
  (if (= 1 (count args))
    (if-let [solution (solutions (first args))]
      (let [[sol1 sol2] (solution)]
        (println sol1)
        (println sol2))
      (usage))
    (usage)))
