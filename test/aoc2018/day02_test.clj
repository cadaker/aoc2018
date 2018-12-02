(ns aoc2018.day02_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day02]))

(deftest letter-count-test
  (testing "letter-count-modifier"
    (is (= 1 (letter-count-modifier {:a 1 :b 2} 1)))
    (is (= 1 (letter-count-modifier {:a 1 :b 2} 2)))
    (is (= 0 (letter-count-modifier {:a 1 :b 2} 3)))
    ))

(deftest compute-factors-test
  (testing "compute-factors"
    (is (= [1 0] (compute-factors '({:a 2}))))
    (is (= [0 1] (compute-factors '({:a 3}))))
    (is (= [0 0] (compute-factors '({:a 1}))))
    (is (= [1 1] (compute-factors '({:a 2 :b 3}))))
    (is (= [1 1] (compute-factors '({:a 2} {:b 3}))))
    ))

(deftest count-if-test
  (testing "count-if"
    (is (= 0 (count-if true? '(false false false))))
    (is (= 1 (count-if true? '(false true false))))
    (is (= 2 (count-if (partial = 1) '(1 2 3 2 1))))
    (is (= 3 (count-if = '(1 2 3 2 1) '(1 1 3 2 2))))
    ))

(deftest id-match-test
  (testing "id-match"
    (is (not (id-match "abcde" "abcde")))
    (is (not (id-match "abcde" "axcye")))
    (is (not (id-match "klmno" "pqrst")))
    (is (id-match "fghij" "fguij"))
    (is (id-match "a" "b"))
    (is (id-match "xa" "xb"))
    (is (id-match "ax" "bx"))
    (is (id-match "xax" "xbx"))
    ))

(deftest get-common-test
  (testing "get-common"
    (is (= "fgij" (clojure.string/join (get-common "fghij" "fguij"))))
    ))
