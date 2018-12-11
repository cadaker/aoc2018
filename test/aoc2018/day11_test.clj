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

(deftest int-img-test
  (testing "int-img"
    (is (= (integral-image (partial apply +) 3 3)
           {[0 0] 0, [1 0] 0, [2 0] 0,  [3 0] 0
            [0 1] 0, [1 1] 2, [2 1] 5,  [3 1] 9,
            [0 2] 0, [1 2] 5, [2 2] 12, [3 2] 21,
            [0 3] 0, [1 3] 9, [2 3] 21, [3 3] 36}))
    (let [img (integral-image (partial power-level serial-number) 300 300)]
      (is (= (power-of-grid serial-number 1 1 [1 1])
             (img-grid-sum img 1 1 [1 1])))
      (is (= (power-of-grid serial-number 4 4 [2 3])
             (img-grid-sum img 4 4 [2 3])))
      (is (= (power-of-grid serial-number 3 3 [17 23])
             (img-grid-sum img 3 3 [17 23])))
      )))
