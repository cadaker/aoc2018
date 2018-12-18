(ns aoc2018.day12_test
  (:require [clojure.test :refer :all])
  (:require [aoc2018.cycle :as cycle])
  (:use [aoc2018.day12]))

(def test-rules-base
  { (vec "...##") \#
    (vec "..#..") \#
    (vec ".#...") \#
    (vec ".#.#.") \#
    (vec ".#.##") \#
    (vec ".##..") \#
    (vec ".####") \#
    (vec "#.#.#") \#
    (vec "#.###") \#
    (vec "##.#.") \#
    (vec "##.##") \#
    (vec "###..") \#
    (vec "###.#") \#
    (vec "####.") \# })

(defn extend-rules [rules key]
  (get rules key \.))

(def test-rules (partial extend-rules test-rules-base))

(def test-initial-state "#..#.#..##......###...###")

(deftest trim-pots-test
  (testing "trim-pots"
    (is (= (trim-pots "..##.#...###.")
           [(seq "##.#...###") 2]))))

(deftest transform-pots-test
  (testing "transform-pots"
    (is (= (transform-pots (partial extend-rules {(vec "..#..") \#}) ["#" 1])
           [(seq "#") 1]))
    (is (= (transform-pots (partial extend-rules {(vec "...#.") \#}) ["#" 0])
           [(seq "#") -1]))
    (is (= (transform-pots (partial extend-rules {(vec ".#...") \#}) ["#" 0])
           [(seq "#") 1]))
        ))

(deftest test-input
  (testing "test-input"
    (let [generations (iterate (partial transform-pots test-rules) [(seq test-initial-state) 0])]
      (is (= (nth generations 0)
             [(seq "#..#.#..##......###...###") 0]))
      (is (= (nth generations 1)
             [(seq "#...#....#.....#..#..#..#") 0]))
      (is (= (nth generations 2)
             [(seq "##..##...##....#..#..#..##") 0]))
      (is (= (nth generations 3)
             [(seq "#.#...#..#.#....#..#..#...#") -1]))
      (is (= (nth generations 4)
             [(seq "#.#..#...#.#...#..#..##..##") 0]))
      (is (= (nth generations 5)
             [(seq "#...##...#.#..#..#...#...#") 1]))
      (is (= (nth generations 6)
             [(seq "##.#.#....#...#..##..##..##") 1]))
      (is (= (nth generations 7)
             [(seq "#..###.#...##..#...#...#...#") 0]))
      (is (= (nth generations 8)
             [(seq "#....##.#.#.#..##..##..##..##") 0]))
      (is (= (nth generations 9)
             [(seq "##..#..#####....#...#...#...#") 0]))
      (is (= (nth generations 10)
             [(seq "#.#..#...#.##....##..##..##..##") -1]))
      (is (= (nth generations 11)
             [(seq "#...##...#.#...#.#...#...#...#") 0]))
      (is (= (nth generations 12)
             [(seq "##.#.#....#.#...#.#..##..##..##") 0]))
      (is (= (nth generations 13)
             [(seq "#..###.#....#.#...#....#...#...#") -1]))
      (is (= (nth generations 14)
             [(seq "#....##.#....#.#..##...##..##..##") -1]))
      (is (= (nth generations 15)
             [(seq "##..#..#.#....#....#..#.#...#...#") -1]))
      (is (= (nth generations 16)
             [(seq "#.#..#...#.#...##...#...#.#..##..##") -2]))
      (is (= (nth generations 17)
             [(seq "#...##...#.#.#.#...##...#....#...#") -1]))
      (is (= (nth generations 18)
             [(seq "##.#.#....#####.#.#.#...##...##..##") -1]))
      (is (= (nth generations 19)
             [(seq "#..###.#..#.#.#######.#.#.#..#.#...#") -2]))
      (is (= (nth generations 20)
             [(seq "#....##....#####...#######....#.#..##") -2]))
      )))

(deftest find-cycle-test
  (testing "find-cycle"
    (is (= [0 1] (cycle/find-cycle '(0 0 0 0 0 0))))
    (is (= [1 2] (cycle/find-cycle '(0 1 1 0 0 0))))
    (is (= [1 5] (cycle/find-cycle '(0 1 2 3 4 1))))
    ))

(deftest eliminate-cycle-test
  (testing "eliminate-cycle"
    (is (= [0 10] (cycle/eliminate-cycle 0 1 10)))
    (is (= [0 5] (cycle/eliminate-cycle 0 2 10)))
    (is (= [2 4] (cycle/eliminate-cycle 1 3 10)))
    (is (= [1 3] (cycle/eliminate-cycle 0 3 10)))
    (is (= [1 3] (cycle/eliminate-cycle 1 4 10)))
    (is (= [3 12] (cycle/eliminate-cycle 3 4 15)))
    ))
