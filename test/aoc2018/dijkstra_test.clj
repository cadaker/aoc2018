(ns aoc2018.dijkstra_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.dijkstra]))


(deftest dijkstra-structure-test
  (testing "dijkstra-structure"
    (let [d0 empty-dijkstra
          d1 (dijkstra-update d0 3 :foo)
          d2 (dijkstra-update d1 2 :bar)
          d3 (dijkstra-update d2 1 :foo)]
      (is (= d1 {:lookup {:foo 3}, :boundary #{[3 :foo]}}))
      (is (= d2 {:lookup {:foo 3 :bar 2}, :boundary #{[2 :bar] [3 :foo]}}))
      (is (= d3 {:lookup {:foo 1 :bar 2}, :boundary #{[1 :foo] [2 :bar]}}))
      (is (= [3 :foo] (dijkstra-peek d1)))
      (is (= [2 :bar] (dijkstra-peek d2)))
      (is (= [1 :foo] (dijkstra-peek d3)))
      (is (= (dijkstra-pop d1) {:lookup {}, :boundary #{}}))
      (is (= (dijkstra-pop d2) {:lookup {:foo 3}, :boundary #{[3 :foo]}}))
      (is (= (dijkstra-pop d3) {:lookup {:bar 2}, :boundary #{[2 :bar]}}))
      )))

(deftest dijkstra-test
  (testing "dijkstra"
    (let [steps-fn (fn [_ x]
                    [nil [[(* x x) (inc x)]]])]
      (is (= 14 (do-dijkstra 0 4 steps-fn nil))))))
