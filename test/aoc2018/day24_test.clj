(ns aoc2018.day24_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day24]))

(deftest parse-unit-test
  (testing "parse-unit"
    (is (= (parse-unit "18 units each with 729 hit points (weak to fire; immune to cold, slashing) with an attack that does 8 radiation damage at initiative 10")
           '{:count 18 :hp 729 :weak-to ["fire"] :immune-to ["cold" "slashing"] :attack 8 :attack-type "radiation" :initiative 10}))))
