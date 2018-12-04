(ns aoc2018.day04
  (:use aoc2018.driver))

(def guard-pattern #"\[\d+-\d+-\d+ \d+:\d+] Guard #(\d+) begins shift")
(def sleep-pattern #"\[\d+-\d+-\d+ \d+:(\d+)] falls asleep")
(def wake-pattern  #"\[\d+-\d+-\d+ \d+:(\d+)] wakes up")

(defn parse-line [line]
  (let [guard-match (re-matches guard-pattern line)
        wake-match  (re-matches wake-pattern line)
        sleep-match (re-matches sleep-pattern line)
        get-param #(Integer/valueOf (second %))]
    (cond
     guard-match {:type :guard, :id (get-param guard-match)}
     sleep-match {:type :sleep, :t (get-param sleep-match)}
     wake-match {:type :wake, :t (get-param wake-match)})))

(defsolution day04 [input]
  (let [log (map parse-line (sort (clojure.string/split-lines input)))]
    [(first log)
     0]))
