(ns aoc2018.day04_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day04]))

(deftest parse-line-test
  (testing "parse-line"
    (is (= {:type :sleep, :t 21} (parse-line "[1518-11-07 00:21] falls asleep")))
    (is (= {:type :wake, :t 44} (parse-line "[1518-07-01 00:44] wakes up")))
    (is (= {:type :guard, :id 2909} (parse-line "[1518-03-15 23:57] Guard #2909 begins shift")))
    ))

(deftest update-schedule-test
  (testing "update-schedule"
    (is (= (update-schedule {} 3 4 9)
           {3 {4 1, 5 1, 6 1, 7 1, 8 1}}))
    (is (= (update-schedule {1 {2 1, 3 1, 4 1}} 1 3 7)
           {1 {2 1, 3 2, 4 2, 5 1, 6 1}}))))

(deftest build-schedules-test
  (testing "build-schedules"
    (is (= (build-schedules '({:type :guard :id 5}
                              {:type :sleep :t 5}
                              {:type :wake :t 7}
                              {:type :guard :id 2}
                              {:type :guard :id 3}
                              {:type :sleep :t 19}
                              {:type :wake :t 27}
                              {:type :guard :id 5}
                              {:type :sleep :t 6}
                              {:type :wake :t 9}))
           {3 {19 1, 20 1, 21 1, 22 1, 23 1, 24 1, 25 1, 26 1}
            5 {5 1, 6 2, 7 1, 8 1}}))
    ))
