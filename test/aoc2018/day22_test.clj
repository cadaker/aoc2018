(ns aoc2018.day22_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day22]))

(deftest geologic-index-test
  (testing "geologic-index"
    (let [depth 510
          target-xy {:x 10 :y 10}
          geo-index (partial compute-geologic-index target-xy)]
      (is (= 0 (geo-index {} {:x 0 :y 0})))
      (is (= 0 (geo-index {} target-xy)))
      (is (= 16807 (geo-index {} {:x 1 :y 0})))
      (is (= 48271 (geo-index {} {:x 0 :y 1})))
      (is (= 145722555 (geo-index {{:x 1 :y 0} 17317, {:x 0 :y 1} 8415} {:x 1 :y 1}))))))

(deftest erosion-test
  (testing "erosion"
    (let [erosions (compute-erosions 510 {:x 10 :y 10} 10 10)]
      (is (= 510 (erosions {:x 0 :y 0})))
      (is (= 17317 (erosions {:x 1 :y 0})))
      (is (= 8415 (erosions {:x 0 :y 1})))
      (is (= 1805 (erosions {:x 1 :y 1})))
      (is (= 510 (erosions {:x 10 :y 10}))))))

(deftest cave-type-test
  (testing "cave-type"
    (is (= :rocky (cave-type 510)))
    (is (= :wet (cave-type 17317)))
    (is (= :rocky (cave-type 8415)))
    (is (= :narrow (cave-type 1805)))
    ))

(deftest total-risk-test
  (testing "total-risk"
    (is (= 114 (total-risk (compute-erosions 510 {:x 10 :y 10} 10 10))))))
