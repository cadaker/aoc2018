(ns aoc2018.day25_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day25]))

(def example1 '([0 0 0 0] [3 0 0 0] [0 3 0 0] [0 0 3 0] [0 0 0 3] [0 0 0 6] [9 0 0 0] [12 0 0 0]))
(def example2 '([-1 2 2 0] [0 0 2 -2] [0 0 0 -2] [-1 2 0 0] [-2 -2 -2 2]
                [3 0 2 -1] [-1 3 2 2 ] [-1 0 -1 0] [0 2 1 -2] [3 0 0 0]))
(def example3 '([1 -1 0 1] [2 0 -1 0] [3 2 -1 0] [0 0 3 1] [0 0 -1 -1]
                [2 3 -2 0] [-2 2 0 0] [2 -2 0 -1] [1 -1 0 -1] [3 2 0 2]))
(def example4 '([1 -1 -1 -2] [-2 -2 0 1] [0 2 1 3] [-2 3 -2 1] [0 2 3 -2]
                [-1 -1 1 -2] [0 -2 -1 0] [-2 2 3 -1] [1 2 2 0] [-1 -2 0 -2]))


(deftest examples-test
  (testing "example1"
    (is (= 2 (count-constellations (process-points example1))))
    (is (= 1 (count-constellations (process-points (conj example1 [6 0 0 0]))))))
  (testing "example2"
    (is (= 4 (count-constellations (process-points example2)))))
  (testing "example3"
    (is (= 3 (count-constellations (process-points example3)))))
  (testing "example4"
    (is (= 8 (count-constellations (process-points example4))))))
