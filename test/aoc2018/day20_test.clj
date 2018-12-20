(ns aoc2018.day20_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day20]))

(deftest add-edge-test
  (testing "add-edge"
    (is (= {[0 0] #{[0 1]}, [0 1] #{[0 0]}} (add-edge empty-graph [0 0] [0 1])))
    ))

(deftest parsing-test
  (testing "parsing"
    (is (= (build-graph "^WNE$")
           {[0 0] #{[0 -1]}, [0 -1] #{[0 0] [-1 -1]}, [-1 -1] #{[0 -1] [-1 0]}, [-1 0] #{[-1 -1]}}))
    (is (= (build-graph "^(S|E)S$")
           {[0 0] #{[1 0] [0 1]}, [1 0] #{[0 0] [2 0]}, [2 0] #{[1 0]},
            [0 1] #{[0 0] [1 1]}, [1 1] #{[0 1]}}))
    ))

(deftest formatting-test
  (testing "formatting"
    (is (= (format-graph (build-graph "^WNE$"))
".|.
-##
.|."))
    ))

(def test-input-1 "^ENWWW(NEEE|SSE(EE|N))$")
(def test-map-1
".|.|.|.
-######
.|.|.|.
-#####-
.#.#.|.
-#-####
.|.|.|.")

(def test-input-2 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")
(def test-map-2
".|.#.|.#.
-###-#-#-
.|.|.#.#.
-#####-#-
.#.#.|.#.
-#-#####-
.#.|.|.|.
-###-###-
.|.|.#.|.")

(deftest input-test
  (testing "input"
    (is (= (format-graph (build-graph test-input-1)) test-map-1))
    (is (= (format-graph (build-graph test-input-2)) test-map-2))))
