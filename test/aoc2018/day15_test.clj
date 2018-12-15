(ns aoc2018.day15_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day15]))

(deftest read-map-test
  (testing "read-map"
    (let [map-input
"#######
#.G.E.#
#E.G.E#
#.G.E.#
#######"]
      (is (= (read-map map-input)
             [{[0 0] \# [0 1] \# [0 2] \# [0 3] \# [0 4] \# [0 5] \# [0 6] \# 
               [1 0] \# [1 1] \. [1 2] \. [1 3] \. [1 4] \. [1 5] \. [1 6] \# 
               [2 0] \# [2 1] \. [2 2] \. [2 3] \. [2 4] \. [2 5] \. [2 6] \# 
               [3 0] \# [3 1] \. [3 2] \. [3 3] \. [3 4] \. [3 5] \. [3 6] \# 
               [4 0] \# [4 1] \# [4 2] \# [4 3] \# [4 4] \# [4 5] \# [4 6] \#}
              {[1 2] {:type :goblin :hp HP} [1 4] {:type :elf :hp HP}
               [2 1] {:type :elf :hp HP} [2 3] {:type :goblin :hp HP} [2 5] {:type :elf :hp HP}
               [3 2] {:type :goblin :hp HP} [3 4] {:type :elf :hp HP}}])))))

(let [test-input (read-map "#######\n#E..G.#\n#...#.#\n#.G.#G#\n#######\n")]
  (def test-cave-1 (first test-input))
  (def test-units-1 (second test-input)))

(deftest neighbours-test
  (testing "neighbours"
    (is (= (neighbours test-cave-1 test-units-1 [0 0]) '()))
    (is (= (neighbours test-cave-1 test-units-1 [1 1]) '([1 2] [2 1])))
    (is (= (neighbours test-cave-1 test-units-1 [2 2]) '([1 2] [2 1] [2 3])))
    ))

(deftest target-positions-test
  (testing "target-positions"
    (is (= (target-positions test-cave-1 test-units-1 :elf [1 1])
           #{[1 3] [1 5] [2 2] [2 5] [3 1] [3 3]}))))

(deftest find-move-test
  (testing "find-move"
    (is (= (find-move test-cave-1 test-units-1 :elf [1 1]) [1 2]))
    (let [[cave units] (read-map "#######\n#.E...#\n#.....#\n#...G.#\n#######\n")]
      (is (= (find-move cave units :elf [1 2]) [1 3])))
    (let [[cave units] (read-map "#####\n#G..#\n#...#\n#..E#\n#####\n")]
      (is (= (find-move cave units :goblin [1 1]) [1 2])))))

(let [test-input (read-map
"#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########")]
  (def test-cave-2 (first test-input))
  (def test-units-2 (second test-input)))

(deftest neighbour-target-test
  (testing "neighbour-target"
    (is (= (neighbour-target test-cave-2 {[2 4] {:type :goblin :hp HP} [3 4] {:type :elf :hp HP}} :goblin [2 4])
           [3 4]))))

(deftest movement-test
  (testing "movement-test"
    (let [rounds (iterate (partial do-round test-cave-2) test-units-2)
          G (fn [hp] {:type :goblin :hp hp})
          E (fn [hp] {:type :elf :hp hp})]
      (is (= (nth rounds 1)
             {[1 2] (G HP) [1 6] (G HP) [2 4] (G HP) [3 4] (E HP) [3 7] (G HP)
              [4 2] (G HP) [6 1] (G HP) [6 4] (G HP) [6 7] (G HP)}))
      (is (= (nth rounds 2)
             {[1 3] (G HP) [1 5] (G HP) [2 4] (G HP) [3 4] (E HP) [3 6] (G HP)
              [3 2] (G HP) [5 1] (G HP) [5 4] (G HP) [5 7] (G HP)}))
      (is (= (nth rounds 3)
             {[2 3] (G HP) [2 4] (G HP) [2 5] (G HP) [3 4] (E HP) [3 5] (G HP)
              [3 3] (G HP) [4 1] (G HP) [4 4] (G HP) [5 7] (G HP)}))
      )))
