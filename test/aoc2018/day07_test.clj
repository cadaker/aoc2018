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

(deftest job-queue-test
  (testing "job-queue"
    (let [q (make-job-queue)
          q1 (enqueue-job q "A" 1)
          q1+ (work-job-queue q1)
          q2 (enqueue-job q1 "B" 2)
          q2+ (work-job-queue q2)
          q2++ (work-job-queue q2+)]
      (is (job-queue-empty? q))
      (is (zero? (job-queue-size q)))
      (is (= 1 (job-queue-size q1)))
      (is (= 2 (job-queue-size q2)))
      (is (empty? (second (dequeue-done q2))))
      (is (= q2 (first (dequeue-done q2))))

      (is (= '("A") (second (dequeue-done q1+))))
      (is (= {:t 1, :elems {}} (first (dequeue-done q1+))))

      (is (= '("A") (second (dequeue-done q2+))))
      (is (= {:t 1, :elems {"B" 2}} (first (dequeue-done q2+))))
      )))

(deftest work-elves-test
  (testing "work-elves"
    (is (= 15 (work-elves test-graph
                          2
                          (partial job-time 0))))))
