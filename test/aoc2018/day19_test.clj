(ns aoc2018.day19_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day19]))

(def test-program
  [["seti" 5 0 1]
   ["seti" 6 0 2]
   ["addi" 0 1 0]
   ["addr" 1 2 3]
   ["setr" 1 0 0]
   ["seti" 8 0 4]
   ["seti" 9 0 5]])

(deftest step-program-test
  (testing "step-program"
    (let [steps (iterate (fn [[vm ip]] (step-program 0 test-program vm ip))
                         [{:regs [0 0 0 0 0 0]} 0])]
      (is (= (nth steps 1) [{:regs [0 5 0 0 0 0]} 1]))
      (is (= (nth steps 2) [{:regs [1 5 6 0 0 0]} 2]))
      (is (= (nth steps 3) [{:regs [3 5 6 0 0 0]} 4]))
      (is (= (nth steps 4) [{:regs [5 5 6 0 0 0]} 6]))
      (is (= (nth steps 5) [{:regs [6 5 6 0 0 9]} 7]))
      )))
