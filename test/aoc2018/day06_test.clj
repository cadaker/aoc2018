(ns aoc2018.day06_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day06]))

(deftest parse-point-test
  (testing "parse-point"
    (is (= {:x 19, :y 27} (parse-point "19, 27")))))

(deftest bounded-in-dir?-test
  (testing "bounded-in-dir?"
    (is (bounded-in-dir? {:x 0 :y 0} {:x 1 :y 0} :right))
    (is (not (bounded-in-dir? {:x 0 :y 0} {:x 1 :y 0} :left)))
    (is (not (bounded-in-dir? {:x 0 :y 0} {:x 1 :y 0} :up)))
    (is (not (bounded-in-dir? {:x 0 :y 0} {:x 1 :y 0} :down)))
    (is (bounded-in-dir? {:x 0 :y 0} {:x 1 :y 1} :right))
    (is (bounded-in-dir? {:x 0 :y 0} {:x 1 :y 1} :up))
    (is (not (bounded-in-dir? {:x 0 :y 0} {:x 1 :y 1} :down)))
    (is (not (bounded-in-dir? {:x 0 :y 0} {:x 1 :y 1} :left)))
    ))

(def points '({:x 1 :y 1}
              {:x 1 :y 6}
              {:x 8 :y 3}
              {:x 3 :y 4}
              {:x 5 :y 5}
              {:x 8 :y 9}))

(deftest bounded-in?-test
  (testing "bounded-in?"
    (is (not (bounded-in? {:x 1 :y 1} points)))
    (is (not (bounded-in? {:x 1 :y 6} points)))
    (is (not (bounded-in? {:x 8 :y 3} points)))
    (is (bounded-in? {:x 3 :y 4} points))
    (is (bounded-in? {:x 5 :y 5} points))
    (is (not (bounded-in? {:x 8 :y 9} points)))))
