(ns aoc2018.day08_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day08]))

(def test-tree '{:children ({:children () :meta (10 11 12)}
                            {:children ({:children () :meta (99)}) :meta (2)})
                 :meta (1 1 2)})

(deftest parse-tree-test
  (testing "parse-tree"
    (is (= (parse-tree '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))
           test-tree))))

(deftest meta-sum-test
  (testing "meta-sum"
    (is (= (meta-sum test-tree) 138))))
