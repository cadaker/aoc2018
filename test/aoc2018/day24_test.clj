(ns aoc2018.day24_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day24]))

(deftest parse-unit-test
  (testing "parse-unit"
    (is (= (parse-unit "18 units each with 729 hit points (weak to fire; immune to cold, slashing) with an attack that does 8 radiation damage at initiative 10")
           '{:count 18 :hp 729 :weak-to ["fire"] :immune-to ["cold" "slashing"] :attack 8 :attack-type "radiation" :initiative 10}))))

(deftest attack-order-test
  (testing "attack-order"
    (let [units  {0 {:count 3 :attack 4 :initiative 5}
                  1 {:count 4 :attack 3 :initiative 4}
                  2 {:count 1 :attack 2 :initiative 4}
                  3 {:count 2 :attack 1 :initiative 5}}]
      (is (= (attack-order units) '(0 1 3 2))))))

(deftest damage-test
  (testing "damage"
    (is (= 10 (damage {:count 5 :attack 2 :attack-type "foo"}
                      {:weak-to nil :immune-to nil})))
    (is (= 20 (damage {:count 5 :attack 2 :attack-type "foo"}
                      {:weak-to ["foo"] :immune-to nil})))
    (is (= 0  (damage {:count 5 :attack 2 :attack-type "foo"}
                      {:weak-to nil :immune-to ["foo"]})))
    ))

(deftest target-order-test
  (testing "target-order"
    (let [defenders {0 {:count 5 :attack 2 :weak-to nil :immune-to nil :initiative 5}
                     1 {:count 5 :attack 2 :weak-to nil :immune-to ["foo"] :initiative 5}
                     2 {:count 5 :attack 2 :weak-to ["foo"] :immune-to nil :initiative 5}
                     3 {:count 5 :attack 3 :weak-to nil :immune-to nil :initiative 5}
                     4 {:count 5 :attack 5 :weak-to nil :immune-to nil :initiative 5}
                     5 {:count 10 :attack 2 :weak-to nil :immune-to nil :initiative 5}
                     6 {:count 5 :attack 2 :weak-to nil :immune-to nil :initiative 3}
                     7 {:count 5 :attack 2 :weak-to nil :immune-to nil :initiative 4}}]
      (is (= (target-order {:count 1 :attack 10 :attack-type "foo"} defenders)
             '(2 4 5 3 0 7 6 1))))
    (let [defenders {0 {:count 10 :attack 10 :weak-to nil :immune-to nil :initiative 5}
                     1 {:count 10 :attack 10 :weak-to nil :immune-to nil :initiative 4}
                     2 {:count 10 :attack 10 :weak-to nil :immune-to nil :initiative 3}}]
      (is (= (target-order {:count 10 :attack 5 :attack-type "foo" :initiative 6} defenders)
             '(0 1 2))))
    ))

(deftest target-selection-test
  (testing "damage-prio"
    (is (= (target-selection
            {0 {:count 10 :attack 5 :attack-type "foo" :initiative 5}
             1 {:count 10 :attack 5 :attack-type "foo" :initiative 6}
             2 {:count 10 :attack 4 :attack-type "foo" :initiative 3}
             3 {:count 5  :attack 8 :attack-type "foo" :initiative 4}}
            {0 {:count 10 :attack 10 :weak-to ["foo"] :immune-to nil :initiative 5}
             1 {:count 10 :attack 10 :weak-to nil :immune-to nil :initiative 4}
             2 {:count 10 :attack 10 :weak-to nil :immune-to ["foo"] :initiative 3}})
           {1 0, 0 1, 3 2, 2 nil})))
  (testing "power-prio"
    (is (= (target-selection
            {0 {:count 10 :attack 5 :attack-type "foo" :initiative 5}
             1 {:count 10 :attack 5 :attack-type "foo" :initiative 6}
             2 {:count 10 :attack 4 :attack-type "foo" :initiative 3}
             3 {:count 5  :attack 8 :attack-type "foo" :initiative 4}}
            {0 {:count 12 :attack 10 :weak-to nil :immune-to nil :initiative 5}
             1 {:count 10 :attack 15 :weak-to nil :immune-to nil :initiative 4}
             2 {:count 10 :attack 16 :weak-to nil :immune-to nil :initiative 3}})
           {1 2, 0 1, 3 0, 2 nil})))
  (testing "initiative-prio"
    (is (= (target-selection
            {0 {:count 10 :attack 5 :attack-type "foo" :initiative 5}
             1 {:count 10 :attack 5 :attack-type "foo" :initiative 6}
             2 {:count 10 :attack 4 :attack-type "foo" :initiative 3}
             3 {:count 5  :attack 8 :attack-type "foo" :initiative 4}}
            {0 {:count 10 :attack 10 :weak-to nil :immune-to nil :initiative 5}
             1 {:count 10 :attack 10 :weak-to nil :immune-to nil :initiative 4}
             2 {:count 10 :attack 10 :weak-to nil :immune-to nil :initiative 3}})
           {1 0, 0 1, 3 2, 2 nil})))
  )
