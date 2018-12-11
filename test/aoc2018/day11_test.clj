(ns aoc2018.day11_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day11]))

(deftest hundred-digit-test
  (testing "hundred-digit"
    (is (= 9 (hundred-digit 949)))
    (is (= 3 (hundred-digit 12345)))
    (is (= 3 (hundred-digit -12345)))
    ))

(deftest power-level-test
  (testing "power-level"
    (is (= 4 (power-level 8 [3 5])))
    (is (= -5 (power-level 57 [122 79])))
    (is (= 0 (power-level 39 [217 196])))
    (is (= 4 (power-level 71 [101 153])))
    ))
