(ns aoc2018.day23_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day23]))

(deftest clique-test
  (testing "clique?"
    (let [graph  {1 #{1 3 4} 2 #{2 3 4} 3 #{1 2 3 4} 4 #{1 2 3 4}}]
      (is (clique? graph #{1 3 4}))
      (is (clique? graph #{1 3}))
      (is (clique? graph #{1}))
      (is (clique? graph #{3 4}))
      (is (clique? graph #{2 3 4}))
      (is (not (clique? graph #{1 2 3 4})))
      (is (not (clique? graph #{1 2 3})))
      (is (not (clique? graph #{1 2 4}))))))

(deftest mh-expand-test
  (testing "mh-expand-step"
    (is (= (mh-expand-step [] [0 0 0] 0)
           '([0 0 0]))))
  (testing "mh-expand"
    (is (= (take 10 (mh-expand [0 0 0]))
           (list [0 0 0]
                 [0 0 1]
                 [0 1 0]
                 [1 0 0]
                 [0 0 2]
                 [0 1 1]
                 [0 2 0]
                 [1 0 1]
                 [1 1 0]
                 [2 0 0])))))
