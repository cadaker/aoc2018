(ns aoc2018.day14
  (:require clojure.set)
  (:use aoc2018.driver))

(defn digits-of [n]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def input 440231)

(defsolution day14 [_]
  [(steps-after input)
   0])
