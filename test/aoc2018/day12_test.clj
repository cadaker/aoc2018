(ns aoc2018.day12_test
  (:require [clojure.test :refer :all])
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
    (is (= (transform-pots (partial extend-rules {(vec "..#..") \#}) "#" 1)
           [(seq "#") 1]))
    (is (= (transform-pots (partial extend-rules {(vec "...#.") \#}) "#" 0)
           [(seq "#") -1]))
    (is (= (transform-pots (partial extend-rules {(vec ".#...") \#}) "#" 0)
           [(seq "#") 1]))
        ))

(deftest test-input
  (testing "test-input"
    (let [all-generations (iterate (fn [[pots start-index]]
                                     (transform-pots test-rules pots start-index))
                                   [(seq test-initial-state) 0])
          generations (take 21 all-generations)]
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
