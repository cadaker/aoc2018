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
  (let [update-one (fn [sched guard t]
                     (update-in sched [guard t] #(inc (or % 0))))]
    (reduce (fn [sched t] (update-one sched guard t)) schedules (range start end))))

(defn build-schedules [log-in]
  (loop [schedules {}
         log log-in
         guard nil
         start nil]
    (if (seq log)
      (let [entry (first log)]
        (condp = (:type entry)
          :guard (recur schedules (rest log) (:id entry) nil)
          :sleep (recur schedules (rest log) guard (:t entry))
          :wake (recur
                 (update-schedule schedules guard start (:t entry))
                 (rest log)
                 guard
                 nil)))
      schedules)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn total-time-sleeping [schedule]
  (apply + (vals schedule)))

(defn sleepiest-minute [schedule]
  (first (apply max-key (fn [[k v]] v) schedule)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsolution day04 [input]
  (let [log (map parse-line (sort (clojure.string/split-lines input)))
        schedules (build-schedules log)
        [sleepy-guard sleepy-sched] (apply max-key (fn [[guard schedule]]
                                                     (total-time-sleeping schedule))
                                           schedules)]
    [(* sleepy-guard (sleepiest-minute sleepy-sched))
     0]))
