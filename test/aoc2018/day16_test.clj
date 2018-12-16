(ns aoc2018.day16_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day16]))

(deftest possible-instructions-test
  (testing "possible-instructions"
    (is (= (set (possible-instructions {:pre [3 2 1 1], :post [3 2 2 1] :op [9 2 1 2]}))
           #{"mulr" "addi" "seti"}))))
