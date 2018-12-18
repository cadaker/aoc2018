(ns aoc2018.day18_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day18]))

(def test-initial
".#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.")

(def test-1
".......##.
......|###
.|..|...#.
..|#||...#
..##||.|#|
...#||||..
||...|||..
|||||.||.|
||||||||||
....||..|.")

(def test-2
".......#..
......|#..
.|.|||....
..##|||..#
..###|||#|
...#|||||.
|||||||||.
||||||||||
||||||||||
.|||||||||")


(def test-3
".......#..
....|||#..
.|.||||...
..###|||.#
...##|||#|
.||##|||||
||||||||||
||||||||||
||||||||||
||||||||||")

(deftest evolution-test
  (testing "evolution"
    (let [test-states (iterate evolve (make-lookup test-initial))]
      (is (= (make-lookup test-1) (nth test-states 1)))
      (is (= (make-lookup test-2) (nth test-states 2)))
      (is (= (make-lookup test-3) (nth test-states 3)))
      )))
