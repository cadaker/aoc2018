(ns aoc2018.day09
  (:use aoc2018.driver))

(def starting-marbles '{:front (0) :back nil})

(defn- other-dir-of [dir]
  ({:front :back, :back :front} dir))

(defn push-marble [marbles n]
  (update marbles :front #(cons n %)))

(defn normalize-marbles [marbles dir]
  (if (seq (marbles dir))
    marbles
    (let [other-dir (other-dir-of dir)]
      {dir (reverse (marbles other-dir)), other-dir nil})))

(defn current-marble [marbles]
  (let [marbles' (normalize-marbles marbles :front)]
    (first (:front marbles'))))

(defn pop-current-marble [marbles]
  (let [marbles' (normalize-marbles marbles :front)]
    (update marbles' :front rest)))

(defn- shift-marble [marbles from-key]
  (let [marbles' (normalize-marbles marbles from-key)
        to-key (other-dir-of from-key)]
    (let [current (first (from-key marbles'))
          from' (rest (from-key marbles'))
          to' (cons current (to-key marbles'))]
      {from-key from' to-key to'})))

(defn cw [marbles]
  (shift-marble marbles :front))

(defn ccw [marbles]
  (shift-marble marbles :back))

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

(defn max-score [scores]
  (apply max (vals scores)))

(defsolution day09 [input]
  (let [[player-count marble-count] (read-input input)]
    [(max-score (play player-count marble-count))
     (max-score (play player-count (* 100 marble-count)))]))
