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
              {[1 2] {:type :goblin :hp 200} [1 4] {:type :elf :hp 200}
               [2 1] {:type :elf :hp 200} [2 3] {:type :goblin :hp 200} [2 5] {:type :elf :hp 200}
               [3 2] {:type :goblin :hp 200} [3 4] {:type :elf :hp 200}}])))))

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
      (is (= (find-move cave units :elf [1 2]) [2 2])))))
