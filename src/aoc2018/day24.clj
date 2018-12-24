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

(defsolution day24 [input]
  (let [[imm inf] (parse-input input)]
    [[imm inf]
     0]))
