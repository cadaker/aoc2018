(ns aoc2018.day17_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day17]))

(deftest parse-entry-test
  (testing "parse-entry"
    (is (= [[17 1] [17 2] [17 3]] (parse-entry [] "y=17, x=1..3")))
    (is (= [[1 17] [2 17] [3 17]] (parse-entry [] "x=17, y=1..3")))))

(def test-grid
  (make-grid
   (parse-input (clojure.string/join "\n"
                                     ["x=495, y=2..7"
                                      "y=7, x=495..501"
                                      "x=501, y=3..7"
                                      "x=498, y=2..4"
                                      "x=506, y=1..2"
                                      "x=498, y=10..13"
                                      "x=504, y=10..13"
                                      "y=13, x=498..504"]))))

(def test-input-drawn
"............#.
.#..#.......#.
.#..#..#......
.#..#..#......
.#.....#......
.#.....#......
.#######......
..............
..............
....#.....#...
....#.....#...
....#.....#...
....#######...")

(deftest render-grid-test
  (testing "render-grid"
    (is (= test-input-drawn (render-grid test-grid)))))

(def test-input-1
"......|.....#.
.#..#|||....#.
.#..#~~#......
.#..#~~#......
.#~~~~~#......
.#~~~~~#......
.#######......
..............
..............
....#.....#...
....#.....#...
....#.....#...
....#######...")

(deftest find-bottom-test
  (testing "find-bottom"
    (is (= 7 (find-bottom test-grid [1 500])))
    (is (= 13 (find-bottom test-grid [2 502])))
    (is (= 14 (find-bottom test-grid [2 507])))))

(deftest paint-water-test
  (testing "paint-water"
    (let [[grid spills] (paint-water test-grid [1 500])]
      (is (= test-input-1 (render-grid grid)))
      (is (= spills '([2 502]))))))

(def test-output
"......|.....#.
.#..#||||...#.
.#..#~~#|.....
.#..#~~#|.....
.#~~~~~#|.....
.#~~~~~#|.....
.#######|.....
........|.....
...|||||||||..
...|#~~~~~#|..
...|#~~~~~#|..
...|#~~~~~#|..
...|#######|..")

(deftest paint-all-water-test
  (testing "paint-all-water"
    (is (= test-output (render-grid (paint-all-water test-grid [(starting-point test-grid)]))))
    ))

(deftest sum-water-test
  (testing "sum-water"
    (is (= 57 (sum-water (paint-all-water test-grid [(starting-point test-grid)]))))))
