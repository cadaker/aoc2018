(ns aoc2018.day14_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day14]))

(deftest digits-of-test
  (testing "digits-of"
    (is (= (digits-of 10) '(1 0)))
    ))

(deftest step-test
  (testing "step"
    (is (= (nth steps 0)  [[3 7] 0 1]))
    (is (= (nth steps 1)  [[3 7 1 0] 0 1]))
    (is (= (nth steps 2)  [[3 7 1 0 1 0] 4 3]))
    (is (= (nth steps 3)  [[3 7 1 0 1 0 1] 6 4]))
    (is (= (nth steps 4)  [[3 7 1 0 1 0 1 2] 0 6]))
    (is (= (nth steps 5)  [[3 7 1 0 1 0 1 2 4] 4 8]))
    (is (= (nth steps 6)  [[3 7 1 0 1 0 1 2 4 5] 6 3]))
    (is (= (nth steps 7)  [[3 7 1 0 1 0 1 2 4 5 1] 8 4]))
    (is (= (nth steps 8)  [[3 7 1 0 1 0 1 2 4 5 1 5] 1 6]))
    (is (= (nth steps 9)  [[3 7 1 0 1 0 1 2 4 5 1 5 8] 9 8]))
    (is (= (nth steps 10) [[3 7 1 0 1 0 1 2 4 5 1 5 8 9] 1 13]))
    (is (= (nth steps 11) [[3 7 1 0 1 0 1 2 4 5 1 5 8 9 1 6] 9 7]))
    (is (= (nth steps 12) [[3 7 1 0 1 0 1 2 4 5 1 5 8 9 1 6 7] 15 10]))
    (is (= (nth steps 13) [[3 7 1 0 1 0 1 2 4 5 1 5 8 9 1 6 7 7] 4 12]))
    (is (= (nth steps 14) [[3 7 1 0 1 0 1 2 4 5 1 5 8 9 1 6 7 7 9] 6 2]))
    ))

(deftest step-after-test
  (testing "step-after"
    (is (= '(5 1 5 8 9 1 6 7 7 9) (steps-after 9)))
    (is (= '(0 1 2 4 5 1 5 8 9 1) (steps-after 5)))
    (is (= '(9 2 5 1 0 7 1 0 8 5) (steps-after 18)))
    (is (= '(5 9 4 1 4 2 9 8 8 2) (steps-after 2018)))
    ))

(deftest find-subseq-index-test
  (testing "find-subseq-index"
    (is (= 9 (find-subseq-index '(5 1 5 8 9) (first (nth steps 100)))))
    (is (= nil (find-subseq-index '(5 1 5 8 9) (first (nth steps 4)))))
    (is (= 5 (find-subseq-index '(0 1 2 4 5) (first (nth steps 100)))))
    (is (= 18 (find-subseq-index '(9 2 5 1 0) (first (nth steps 100)))))
    (is (= 2018 (find-subseq-index '(5 9 4 1 4) (first (nth steps 2200)))))
    ))

(deftest search-subseq-test
  (testing "search-subseq"
    (is (= 9 (search-subseq '(5 1 5 8 9))))
    (is (= 5 (search-subseq '(0 1 2 4 5))))
    (is (= 18 (search-subseq '(9 2 5 1 0))))
    (is (= 2018 (search-subseq '(5 9 4 1 4))))
    ))
