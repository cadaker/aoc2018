(ns aoc2018.day09
  (:use aoc2018.driver))

(def starting-marbles '{:front (0) :back nil})

(defn push-marble [marbles n]
  (update marbles :front #(cons n %)))

(defn current-marble [marbles]
  (if (seq (:front marbles))
    (first (:front marbles))
    (last (:back marbles))))

(defn pop-current-marble [marbles]
  (if (seq (:front marbles))
    (update marbles :front rest)
    (recur {:front (reverse (:back marbles)) :back nil})))

(defn shift-marble [marbles from-key to-key]
  (if (seq (from-key marbles))
    (let [current (first (from-key marbles))
          from' (rest (from-key marbles))
          to' (cons current (to-key marbles))]
      {from-key from' to-key to'})
    (recur {from-key (reverse (to-key marbles)) to-key nil} from-key to-key)))

(defn cw [marbles]
  (shift-marble marbles :front :back))

(defn ccw [marbles]
  (shift-marble marbles :back :front))

(defn marble-list [marbles]
  (concat (:front marbles) (reverse (:back marbles))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn game-round [marbles round-no]
  (if (zero? (mod round-no 23))
    (let [ccw7-marbles (ccw (ccw (ccw (ccw (ccw (ccw (ccw marbles)))))))
          removed-marble (current-marble ccw7-marbles)
          marbles' (pop-current-marble ccw7-marbles)]
      [marbles' (+ round-no removed-marble)])
    (let [cw2-marbles (cw (cw marbles))]
      [(push-marble cw2-marbles round-no) 0])))

(defn play [player-count marble-count]
  (loop [marbles starting-marbles
         player 0
         round-no 1
         scores (into {} (for [player (range player-count)]
                           [player 0]))]
    (if (> round-no marble-count)
      scores
      (let [[marbles' score] (game-round marbles round-no)]
        (recur marbles'
               (mod (inc player) player-count)
               (inc round-no)
               (update scores player (partial + score)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def input-pattern #"(\d+) players; last marble is worth (\d+) points\n")

(defn read-input [input]
  (let [[_ player-count marble-count] (re-matches input-pattern input)]
    [(Integer/valueOf player-count) (Integer/valueOf marble-count)]))

(defsolution day09 [input]
  (let [[player-count marble-count] (read-input input)]
    [(apply max (vals (play player-count marble-count)))
     (apply max (vals (play player-count (* 100 marble-count))))]))
