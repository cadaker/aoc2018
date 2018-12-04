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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-schedule [schedules guard start end]
  (let [update-one (fn [sched t]
                     (update-in sched [guard t] #(inc (or % 0))))]
    (reduce update-one schedules (range start end))))

(defn build-schedules [log]
  (let [iter (fn build-schedules-iter [[schedules guard start] entry]
               (case (:type entry)
                 :guard [schedules (:id entry) nil]
                 :sleep [schedules guard (:t entry)]
                 :wake [(update-schedule schedules guard start (:t entry))
                        guard
                        nil]))]
    (first (reduce iter [{} nil nil] log))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn total-time-sleeping [schedule]
  (apply + (vals schedule)))

(defn max-minute-sleep [schedule]
  (apply max (vals schedule)))

(defn find-sleepy-guard [schedules schedule-weight-fn]
  "Returns [guard schedule] for the schedule that gives the highest result on the given fn"
  (apply max-key (comp schedule-weight-fn val) schedules))

(defn sleepiest-minute [schedule]
  (first (apply max-key val schedule)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsolution day04 [input]
  (let [log (map parse-line (sort (clojure.string/split-lines input)))
        schedules (build-schedules log)
        [sleepy-guard-1 sleepy-sched-1] (find-sleepy-guard schedules total-time-sleeping)
        [sleepy-guard-2 sleepy-sched-2] (find-sleepy-guard schedules max-minute-sleep)]
    [(* sleepy-guard-1 (sleepiest-minute sleepy-sched-1))
     (* sleepy-guard-2 (sleepiest-minute sleepy-sched-2))]))
