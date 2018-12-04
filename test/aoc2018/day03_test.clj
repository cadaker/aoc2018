(ns aoc2018.day03_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day03]))

(deftest get-rect-test
  (testing "get-rect"
    (let [input-match (first (re-seq INPUT-PATTERN "#0 @ 1,2: 3x4\n"))]
      (is (= {:id 0, :x 1, :y 2, :w 3, :h 4} (get-rect input-match))))))

(deftest paint-rect-test
  (testing "paint-rect"
    (let [grid0 {[0 0] 0, [1 0] 0, [2 0] 0,
                 [0 1] 0, [1 1] 0, [2 1] 0,
                 [0 2] 0, [1 2] 0, [2 2] 0}
          grid1 {[0 0] 1, [1 0] 1, [2 0] 1,
                 [0 1] 1, [1 1] 1, [2 1] 1,
                 [0 2] 1, [1 2] 1, [2 2] 1}]
      (is (= grid1 (paint-rect grid0 {:x 0, :y 0, :w 3, :h 3}))))))
