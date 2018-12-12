(ns aoc2018.day12
  (:use aoc2018.driver))

(def initial-pattern #"initial state: ([#.]+)")
(def rule-pattern #"([.#]{5}) => ([#.])")

(defn parse-input-line [[rules initial] line]
  (let [initial-match (re-matches initial-pattern line)
        rule-match (re-matches rule-pattern line)]
    (cond
     initial-match (let [[_ state] initial-match]
                     [rules state])
     rule-match (let [[_ k v] rule-match]
                  [(assoc rules (vec k) (first v)) initial])
     :else [rules initial])))

(defn parse-input [lines]
  (reduce parse-input-line [{} nil] lines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn trim-pots [pots]
  (let [count-empty (fn count-empty [xs]
                      (count (take-while (partial = \.) xs)))
        head-count (count-empty pots)
        tail-count (count-empty (reverse pots))]
    [(drop head-count (drop-last tail-count pots))
     head-count]))

(defn transform-pots [rules pots start-index]
  (let [pots' (concat "...." pots "....")
        neighboured-pots (partition 5 1 pots')
        transformed-pots (map rules neighboured-pots)
        [trimmed-pots dropped] (trim-pots transformed-pots)]
    [trimmed-pots (+ start-index -2 dropped)]))

(defn pot-positions [pots head-pos]
  (keep-indexed (fn [ix pot]
                  (when (= pot \#)
                    (+ ix head-pos)))
                pots))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsolution day12 [input]
  (let [[rules initial] (parse-input (clojure.string/split-lines input))
        generations (iterate (fn [[pots start-index]]
                               (transform-pots rules pots start-index))
                             [initial 0])]
    [(let [[pots head-pos] (nth generations 20)]
       (reduce + (pot-positions pots head-pos)))
     0]))
