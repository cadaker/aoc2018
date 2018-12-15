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
    (is (= (target-positions test-cave-1 test-units-1 :elf)
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
          strip-hp (fn [units]
                     (into {} (for [[pos info] units]
                                [pos (dissoc info :hp)])))
          G {:type :goblin}
          E {:type :elf}]
      (is (= (strip-hp (nth rounds 1))
             {[1 2] G [1 6] G [2 4] G [3 4] E [3 7] G
              [4 2] G [6 1] G [6 4] G [6 7] G}))
      (is (= (strip-hp (nth rounds 2))
             {[1 3] G [1 5] G [2 4] G [3 4] E [3 6] G
              [3 2] G [5 1] G [5 4] G [5 7] G}))
      (is (= (strip-hp (nth rounds 3))
             {[2 3] G [2 4] G [2 5] G [3 4] E [3 5] G
              [3 3] G [4 1] G [4 4] G [5 7] G}))
      )))

(let [test-input (read-map
"#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######")]
  (def test-cave-3 (first test-input))
  (def test-units-3 (second test-input)))

(deftest combat-test
  (testing "combat"
    (let [rounds (iterate (partial do-round test-cave-3) test-units-3)
          G (fn [hp] {:type :goblin :hp hp})
          E (fn [hp] {:type :elf :hp hp})]
      (is (= (nth rounds 1)
             {[1 3] (G 200) [2 4] (E 197) [2 5] (G 197) [3 3] (G 200) [3 5] (G 197) [4 5] (E 197)}))
      (is (= (nth rounds 2)
             {[1 4] (G 200) [2 3] (G 200) [2 4] (E 188) [2 5] (G 194) [3 5] (G 194) [4 5] (E 194)}))
      (is (= (nth rounds 23)
             {[1 4] (G 200) [2 3] (G 200) [2 5] (G 131) [3 5] (G 131) [4 5] (E 131)}))
      (is (= (nth rounds 24)
             {[1 3] (G 200) [2 4] (G 131) [3 3] (G 200) [3 5] (G 128) [4 5] (E 128)}))
      (is (= (nth rounds 25)
             {[1 2] (G 200) [2 3] (G 131) [3 5] (G 125) [4 3] (G 200) [4 5] (E 125)}))
      (is (= (nth rounds 26)
             {[1 1] (G 200) [2 2] (G 131) [3 5] (G 122) [4 5] (E 122) [5 3] (G 200)}))
      (is (= (nth rounds 27)
             {[1 1] (G 200) [2 2] (G 131) [3 5] (G 119) [4 5] (E 119) [5 4] (G 200)}))
      (is (= (nth rounds 28)
             {[1 1] (G 200) [2 2] (G 131) [3 5] (G 116) [4 5] (E 113) [5 5] (G 200)}))
      (is (= (nth rounds 47)
             {[1 1] (G 200) [2 2] (G 131) [3 5] (G 59) [5 5] (G 200)}))
      )))

(deftest factions-remaining-test
  (testing "factions-remaining"
    (is (= #{:elf :goblin} (factions-remaining {[0 1] {:type :elf :hp HP}, [3 3] {:type :goblin :hp HP}})))
    (is (= #{:elf} (factions-remaining {[0 1] {:type :elf :hp HP}})))))

(deftest outcomes-test
  (testing "outcomes"
    (is (= 27730 (combat-outcome (fight test-cave-3 test-units-3 default-attack-powers))))
    (let [run (fn [input]
                (let [[cave units] (read-map input)]
                  (combat-outcome (fight cave units default-attack-powers))))]
      (is (= 36334 (run "#######\n#G..#E#\n#E#E.E#\n#G.##.#\n#...#E#\n#...E.#\n#######\n")))
      (is (= 39514 (run "#######\n#E..EG#\n#.#G.E#\n#E.##E#\n#G..#.#\n#..E#.#\n#######\n")))
      (is (= 27755 (run "#######\n#E.G#.#\n#.#G..#\n#G.#.G#\n#G..#.#\n#...E.#\n#######\n")))
      (is (= 28944 (run "#######\n#.E...#\n#.#..G#\n#.###.#\n#E#G#G#\n#...#G#\n#######\n")))
      (is (= 18740 (run "#########\n#G......#\n#.E.#...#\n#..##..G#\n#...##..#\n#...#...#\n#.G...G.#\n#.....G.#\n#########\n")))
      )))

(deftest movement-corner-case-test
  (testing "movement-corner-case"
    ;; If the top goblin kills the left elf, and the second goblin takes its place.
    (let [cave (first (read-map
"#######
#...G##
#..GEE#
#######"))
          units {[1 4] {:type :goblin :hp HP}
                 [2 3] {:type :goblin :hp HP}
                 [2 4] {:type :elf :hp 1}
                 [2 5] {:type :elf :hp HP}}]
      (is (= (do-round cave units)
             {[1 4] {:type :goblin :hp HP}
              [2 4] {:type :goblin :hp 197}
              [2 5] {:type :elf :hp 197}})))))

(deftest pathing-corner-case-test
  (testing "pathing-corner-case"
    ;; First find the spot to go, then find the path. Don't just find a path
    (let [[cave units] (read-map
"#######
#.E...#
#.##..#
#.#...#
#..G..#
#######
")]
      (is (= (target-positions cave units :goblin) #{[1 1] [1 3]}))
      (let [distances (distance-map cave units [4 3])]
        (is (= (distances [1 1]) 5))
        (is (= (distances [1 3]) 5))
        (is (= (distances [4 2]) 1))
        (is (= (distances [4 4]) 1)))
      (is (= [1 1] (find-best-target cave units :goblin [4 3])))
      (is (= [4 2] (find-move cave units :goblin [4 3]))))))

(deftest monotonic-find-first-test
  (testing "monotonic-find-first"
    (is (= 8 (monotonic-find-first #(> % 7))))
    (is (= 17 (monotonic-find-first #(> % 16))))
    (is (= 135 (monotonic-find-first #(> % 134))))))
