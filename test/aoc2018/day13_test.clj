(ns aoc2018.day13_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day13]))

(def test-input
"/->-\\        
|   |  /----\\
| /-+--+-\\  |
| | |  | v  |
\\-+-/  \\-+--/
  \\------/   ")

(let [test-map (parse-input test-input)]
  (def test-rails (first test-map))
  (def test-carts (second test-map)))

(defmacro match-input-line [y xs chs]
  (cons 'do (map (fn [x ch]
                   `(is (= ~ch (test-rails [~x ~y]))))
                 xs
                 (seq chs))))

(deftest parseinput-test
  (testing "parse-input"
    (match-input-line 0 (0 1 2 3 4) "/---\\")
    (match-input-line 1 (0 4 7 8 9 10 11 12) "||/----\\")
    (match-input-line 2 (0 2 3 4 5 6 7 8 9 12) "|/-+--+-\\|")
    (match-input-line 3 (0 2 4 7 9 12) "||||||")
    (match-input-line 4 (0 1 2 3 4 7 8 9 10 11 12) "\\-+-/\\-+--/")
    (match-input-line 5 (2 3 4 5 6 7 8 9) "\\------/")
    (is (= test-carts #{[2 0 :right :left] [9 3 :down :left]}))
    ))

(deftest step-carts-test
  (testing "step-carts"
    (let [steps (iterate (fn [[carts colls]]
                           (step-carts test-rails carts))
                         [test-carts ()])]
      (is (= (nth steps 0)  [#{[2 0 :right :left] [9 3 :down :left]} ()]))
      (is (= (nth steps 1)  [#{[3 0 :right :left] [9 4 :down :left]} ()]))
      (is (= (nth steps 2)  [#{[4 0 :right :left] [10 4 :right :straight]} ()]))
      (is (= (nth steps 3)  [#{[4 1 :down :left] [11 4 :right :straight]} ()]))
      (is (= (nth steps 4)  [#{[4 2 :down :left] [12 4 :right :straight]} ()]))
      (is (= (nth steps 5)  [#{[5 2 :right :straight] [12 3 :up :straight]} ()]))
      (is (= (nth steps 6)  [#{[6 2 :right :straight] [12 2 :up :straight]} ()]))
      (is (= (nth steps 7)  [#{[7 2 :right :straight] [12 1 :up :straight]} ()]))
      (is (= (nth steps 8)  [#{[8 2 :right :right] [11 1 :left :straight]} ()]))
      (is (= (nth steps 9)  [#{[9 2 :right :right] [10 1 :left :straight]} ()]))
      (is (= (nth steps 10) [#{[9 3 :down :right] [9 1 :left :straight]} ()]))
      (is (= (nth steps 11) [#{[9 4 :down :right] [8 1 :left :straight]} ()]))
      (is (= (nth steps 12) [#{[8 4 :left :left] [7 1 :left :straight]} ()]))
      (is (= (nth steps 13) [#{[7 4 :left :left] [7 2 :down :straight]} ()]))
      (is (= (nth steps 14) [#{[7 3 :up :left] [7 3 :down :right]} '([7 3])]))
      )))
