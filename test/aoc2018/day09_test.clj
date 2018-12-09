(ns aoc2018.day09_test
  (:require [clojure.test :refer :all])
  (:use [aoc2018.day09]))

(deftest marbles-test
  (testing "marbles"
    (is (= (push-marble starting-marbles 1)
           '{:front (1 0) :back nil}))
    (is (= (cw '{:front (2 1 0) :back (3 4)})
           '{:front (1 0) :back (2 3 4)}))
    (is (= (cw '{:front nil :back (0 1 2)})
           '{:front (1 0) :back (2)}))
    (is (= 1 (current-marble '{:front (1 0) :back (3 2)})))
    (is (= 1 (current-marble '{:front () :back (0 3 2 1)})))
    (is (= (pop-current-marble '{:front (1 0) :back (3 2)})
           '{:front (0) :back (3 2)}))
    (is (= (pop-current-marble '{:front nil :back (3 2 1)})
           '{:front (2 3) :back nil}))
    ))

(deftest game-round-test
  (testing "game-round"
    (let [rounds (iterate
                  (fn [[marbles score round-no]]
                    (let [[marbles' score'] (game-round marbles round-no)]
                      [marbles' score' (inc round-no)]))
                  [starting-marbles 0 1])
          round-marbles (map (comp marble-list first) rounds)
          round-scores (map second rounds)]
      (is (= (take 6 round-marbles)
             '((0)
               (1 0)
               (2 1 0)
               (3 0 2 1)
               (4 2 1 3 0)
               (5 1 3 0 4 2))))
      (is (= (take 6 round-scores) '(0 0 0 0 0 0)))
      (let [[marbles score]
            (game-round '{:front (22 11 1 12 6 13 3 14 7 15 0 16 8 17 4 18 9 19 2 20 10 21 5)
                          :back nil}
                        23)]
        (is (= (marble-list marbles) '(19 2 20 10 21 5 22 11 1 12 6 13 3 14 7 15 0 16 8 17 4 18)))
        (is (= score 32)))
      )))

(deftest play-test
  (testing "play"
    (is (= (apply max (vals (play 10 1618))) 8317))
    (is (= (apply max (vals (play 13 7999))) 146373))
    (is (= (apply max (vals (play 17 1104))) 2764))
    (is (= (apply max (vals (play 21 6111))) 54718))
    (is (= (apply max (vals (play 30 5807))) 37305))
    ))
