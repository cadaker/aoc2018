(ns aoc2018.day14
  (:require clojure.set)
  (:use aoc2018.driver))

(def digits-of
  {0 [0] 1 [1] 2 [2] 3 [3] 4 [4] 5 [5] 6 [6] 7 [7] 8 [8] 9 [9] 10 [1 0]
   11 [1 1] 12 [1 2] 13 [1 3] 14 [1 4] 15 [1 5] 16 [1 6] 17 [1 7] 18 [1 8]})

(defn digits-of* [n]
  (if (zero? n)
    (list 0)
    (loop [n n, digits ()]
      (if (zero? n)
        digits
        (recur (quot n 10) (cons (rem n 10) digits))))))

(defn step [[recipes elf1 elf2]]
  (let [recipe1 (nth recipes elf1)
        recipe2 (nth recipes elf2)
        digits (digits-of (+ recipe1 recipe2))
        recipes' (apply conj recipes digits)]
    [recipes'
     (rem (+ elf1 1 recipe1) (count recipes'))
     (rem (+ elf2 1 recipe2) (count recipes'))]))

(def steps (iterate step [[3 7] 0 1]))

(defn steps-after [n]
  (let [recipes (ffirst (drop-while #(< (count (first %)) (+ n 10)) steps))]
    (take 10 (drop n recipes))))

(def indexes (iterate inc 0))

(defn find-subseq-index [xs recipes]
  (let [parts (partition (count xs) 1 recipes)
        indexed-parts (map-indexed vector parts)
        matching-parts (filter #(= xs (second %)) indexed-parts)]
    (if (seq matching-parts)
      (ffirst matching-parts)
      nil)))

(defn search-subseq* [xs]
  (loop [n 256, steps steps]
    (println n)
    (let [steps' (drop n steps)]
      (if-let [ix (find-subseq-index xs (ffirst steps'))]
        ix
        (recur (* n 2) steps')))))

(defn safe-subvec [xs start end]
  (if (or (neg? start)
          (> end (count xs)))
    []
    (subvec xs start end)))

(defn search-subseq [xs]
  (loop [state [[3 7] 0 1]]
    (let [[recipes _ _] state
          n (count recipes)
          r1 (safe-subvec recipes (- n (count xs)) n)
          r2 (safe-subvec recipes (- n (count xs) 1) (- n 1))]
      (cond (= xs r1) (- n (count xs))
            (= xs r2) (- n (count xs) 1)
            :else (recur (step state))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def input 440231)
(def input2 [4 4 0 2 3 1])

(defsolution day14 [_]
  [(steps-after input)
   (search-subseq input2)])
