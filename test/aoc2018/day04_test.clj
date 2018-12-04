(ns aoc2018.day04_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day04]))

(deftest parse-line-test
  (testing "parse-line"
    (is (= {:type :sleep, :t 21} (parse-line "[1518-11-07 00:21] falls asleep")))
    (is (= {:type :wake, :t 44} (parse-line "[1518-07-01 00:44] wakes up")))
    (is (= {:type :guard, :id 2909} (parse-line "[1518-03-15 23:57] Guard #2909 begins shift")))
    ))
