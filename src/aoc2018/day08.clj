(ns aoc2018.day08
  (:use aoc2018.driver))

(defn parse-node [[child-count meta-count & data]]
  (loop [child-count child-count, children (), data data]
    (if (zero? child-count)
      [{:children (reverse children), :meta (take meta-count data)}
       (drop meta-count data)]
      (let [[child data'] (parse-node data)]
        (recur (dec child-count)
               (cons child children)
               data')))))

(defn parse-tree [data]
  (first (parse-node data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn meta-sum [tree]
  (let [sum (partial reduce + 0)]
    (+ (sum (:meta tree))
       (sum (map meta-sum (:children tree))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsolution day08 [input]
  (let [data (map #(Integer/valueOf %) (clojure.string/split (clojure.string/trim input) #" "))]
    [(meta-sum (parse-tree data)) 0]))
