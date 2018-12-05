(ns aoc2018.day05_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day05]))

(deftest reaction?-test
  (testing "reaction?"
    (is (reaction? \a \A))
    (is (reaction? \A \a))
    (is (reaction? \x \X))
    (is (not (reaction? \a \a)))
    (is (not (reaction? \A \A)))
    (is (not (reaction? \a \b)))
    (is (not (reaction? \a \B)))
    (is (not (reaction? \A \b)))
    (is (not (reaction? \A \B)))))

(deftest react-test
  (testing "react"
    (is (= "" (react "aA")))
    (is (= "aa" (react "aa")))
    (is (= "ab" (react "ab")))
    (is (= "" (react "abBA")))
    (is (= "abAB" (react "abAB")))
    (is (= "aabAAB" (react "aabAAB")))
    ))

(deftest remove-unit-test
  (testing "remove-unit"
    (is (= (seq "dbcCCBcCcD") (remove-unit "dabAcCaCBAcCcaDA" \a)))))
