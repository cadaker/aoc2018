(ns aoc2018.day01_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day01 :use find-freq-reached-twice]))

(deftest find-freq-test
  (testing "find-freq-reached-twice"
    (is (= 2 (find-freq-reached-twice '(1 -2 3 1))))
    (is (= 0 (find-freq-reached-twice '(1 -1))))
    (is (= 10 (find-freq-reached-twice '(3 3 4 -2 -4))))
    (is (= 5 (find-freq-reached-twice '(-6 3 8 5 -6))))
    (is (= 14 (find-freq-reached-twice '(7 7 -2 -7 -4))))))
