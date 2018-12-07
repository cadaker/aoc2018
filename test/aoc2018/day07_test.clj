(ns aoc2018.day07_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day07]))

(deftest parse-input-test
  (testing "parse-input"
    (is (= {"A" '("C")} (parse-input {} "Step C must be finished before step A can begin.")))
    (is (= {"A" '("C" "B")} (parse-input {"A" '("B")} "Step C must be finished before step A can begin.")))
    (is (= {"B" '("C") "A" '("C")} (parse-input {"B" '("C")} "Step C must be finished before step A can begin.")))
    ))

(deftest all-nodes-test
  (testing "all-nodes"
    (is (= '("A" "B" "C") (all-nodes {"A" '("B" "C")})))))

(deftest remove-from-graph-test
  (testing "remove-from-graph"
    (is (= {"A" '()} (remove-from-graph "B" {"A" '("B")})))
    (is (= {} (remove-from-graph "B" {"B" '("A")})))))

(deftest has-no-dependencies?-test
  (testing "has-no-dependencies"
    (is (has-no-dependencies? "C" {}))
    (is (has-no-dependencies? "C" {"C" ()}))
    (is (has-no-dependencies? "C" {"C" nil}))
    (is (not (has-no-dependencies? "C" {"C" '("A")})))))

(def test-graph
  {"A" '("C")
   "F" '("C")
   "B" '("A")
   "D" '("A")
   "E" '("B" "D" "F")})

(deftest topo-sort-test
  (testing "topo-sort"
    (is (= '("C" "B" "A") (topo-sort {"B" '("C"), "A" '("B")})))
    (is (= '("E" "D" "C" "B" "A") (topo-sort {"B" '("C" "E")
                                              "A" '("B" "E" "D")
                                              "C" '("D" "E")
                                              "D" '("E")})))
    (is (= '("B" "C" "D" "E" "F" "A") (topo-sort {"A" '("E" "D" "B" "C" "F")})))
    (is (= '("F" "A" "B" "C" "D" "E") (topo-sort {"A" '("F")
                                                  "D" '("F")
                                                  "B" '("F")
                                                  "C" '("F")
                                                  "E" '("F")})))
    (is (= '("C" "A" "D" "B") (topo-sort {"A" '("C"), "B" '("D")})))
    (is (= '("C" "A" "B" "D" "F" "E") (topo-sort test-graph)))
    ))
