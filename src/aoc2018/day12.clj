(ns aoc2018.day12
  (:use aoc2018.driver)
  (:require [aoc2018.cycle :as cycle]))

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

(defn transform-pots [rules [pots start-index]]
  (let [pots' (concat "...." pots "....")
        neighboured-pots (partition 5 1 pots')
        transformed-pots (map rules neighboured-pots)
        [trimmed-pots dropped] (trim-pots transformed-pots)]
    [trimmed-pots (+ start-index -2 dropped)]))

(defn pot-positions [[pots head-pos]]
  (keep-indexed (fn [ix pot]
                  (when (= pot \#)
                    (+ ix head-pos)))
                pots))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def N 50000000000)

(defsolution day12 [input]
  (let [[rules initial] (parse-input (clojure.string/split-lines input))
        generations (iterate (partial transform-pots rules) [initial 0])]
    [(reduce + (pot-positions (nth generations 20)))
     (let [[cycle-start cycle-end] (cycle/find-cycle (map first generations))
           [equivalent-generation-no cycles-taken] (cycle/eliminate-cycle cycle-start cycle-end N)
           [pots head-pos] (nth generations equivalent-generation-no)
           cycle-head-movement (- (second (nth generations cycle-end))
                                  (second (nth generations cycle-start)))
           total-head-movement (* cycle-head-movement cycles-taken)]
       (reduce + (pot-positions [pots (+ head-pos total-head-movement)])))]))
