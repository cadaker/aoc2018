(ns aoc2018.day01
  (:use aoc2018.driver))

(defn find-freq-reached-twice [in-diffs]
  (loop [freq 0, diffs (cycle in-diffs), seen #{0}]
    (let [freq' (+ freq (first diffs))]
      (if (contains? seen freq')
        freq'
        (recur freq' (rest diffs) (conj seen freq'))))))

(defsolution day01 [input]
  (let [diffs (map #(Long/valueOf %) (clojure.string/split-lines input))
        freq (reduce + 0 diffs)]
    [freq (find-freq-reached-twice diffs)]))
