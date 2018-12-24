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

(deftest combat-result-test
  (testing "combat-result"
    (let [units {:imm {0 {:count 10}}
                 :inf {0 {:count 13 :hp 10 :initiative 9}
                       1 {:count 12 :hp 8}}}]
      (is (= (combat-result units [:inf 0] 75)
             {:imm {0 {:count 10}}
              :inf {0 {:count 6 :hp 10 :initiative 9}
                    1 {:count 12 :hp 8}}}))
      (is (= (combat-result units [:inf 0] 130)
             {:imm {0 {:count 10}}
              :inf {1 {:count 12 :hp 8}}}))
      (is (= (combat-result units [:inf 0] 500)
             {:imm {0 {:count 10}}
              :inf {1 {:count 12 :hp 8}}}))
      )))

(deftest example-combat
  (let [imm-0 {1 {:count 17 :hp 5390 :immune-to nil :weak-to ["radiation" "bludgeoning"]
                  :attack 4507 :attack-type "fire" :initiative 2}
               2 {:count 989 :hp 1274 :immune-to ["fire"] :weak-to ["bludgeoning" "slashing"]
                  :attack 25 :attack-type "slashing" :initiative 3}}
        inf-0 {1 {:count 801 :hp 4706 :immune-to nil :weak-to ["radiation"]
                  :attack 116 :attack-type "bludgeoning" :initiative 1}
               2 {:count 4485 :hp 2961 :immune-to ["radiation"] :weak-to ["fire" "cold"]
                  :attack 12 :attack-type "slashing" :initiative 4}}
        steps (iterate (fn [[imm inf]] (combat-round imm inf)) [imm-0 inf-0])]
    (testing "round 0"
      (let [[imm inf] (nth steps 0)]
        (is (= (damage (inf 1) (imm 1)) 185832))
        (is (= (damage (inf 1) (imm 2)) 185832))
        (is (= (damage (inf 2) (imm 2)) 107640))
        (is (= (damage (imm 1) (inf 1)) 76619))
        (is (= (damage (imm 1) (inf 2)) 153238))
        (is (= (damage (imm 2) (inf 1)) 24725))
        (is (= (target-selection inf imm) {2 2, 1 1}))
        (is (= (target-selection imm inf) {2 1, 1 2}))
        (is (= (combat-action-order {:imm imm :inf inf}) '([:inf 2] [:imm 2] [:imm 1] [:inf 1])))))
    (testing "round 1"
      (let [[imm inf] (nth steps 1)]
        (is (= (imm 1) nil))
        (is (= (:count (imm 2)) 905))
        (is (= (:count (inf 1)) 797))
        (is (= (:count (inf 2)) 4434))))
    (testing "round 2"
      (let [[imm inf] (nth steps 2)]
        (is (= (imm 1) nil))
        (is (= (:count (imm 2)) 761))
        (is (= (:count (inf 1)) 793))
        (is (= (:count (inf 2)) 4434))))
    (testing "round 3"
      (let [[imm inf] (nth steps 3)]
        (is (= (imm 1) nil))
        (is (= (:count (imm 2)) 618))
        (is (= (:count (inf 1)) 789))
        (is (= (:count (inf 2)) 4434))))
    (testing "round 4"
      (let [[imm inf] (nth steps 4)]
        (is (= (imm 1) nil))
        (is (= (:count (imm 2)) 475))
        (is (= (:count (inf 1)) 786))
        (is (= (:count (inf 2)) 4434))))
    (testing "round 5"
      (let [[imm inf] (nth steps 5)]
        (is (= (imm 1) nil))
        (is (= (:count (imm 2)) 333))
        (is (= (:count (inf 1)) 784))
        (is (= (:count (inf 2)) 4434))))
    (testing "round 6"
      (let [[imm inf] (nth steps 6)]
        (is (= (imm 1) nil))
        (is (= (:count (imm 2)) 191))
        (is (= (:count (inf 1)) 783))
        (is (= (:count (inf 2)) 4434))))
    (testing "round 7"
      (let [[imm inf] (nth steps 7)]
        (is (= (imm 1) nil))
        (is (= (:count (imm 2)) 49))
        (is (= (:count (inf 1)) 782))
        (is (= (:count (inf 2)) 4434))))
    (testing "round 8"
      (let [[imm inf] (nth steps 8)]
        (is (= (imm 1) nil))
        (is (= (imm 2) nil))
        (is (= (:count (inf 1)) 782))
        (is (= (:count (inf 2)) 4434))
        (is (= (total-count inf) 5216))))
    ))
