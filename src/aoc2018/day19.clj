(ns aoc2018.day19
  (:use aoc2018.driver
        [aoc2018.instr :as instr]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ip-pattern #"#ip (\d+)")
(def instr-pattern #"([a-z]+) (\d+) (\d+) (\d+)")

(defn value-of [^String s]
  (Integer/valueOf s))

(defn parse-input [input]
  (loop [lines (clojure.string/split-lines input)
         ip nil
         instrs []]
    (if-let [line (first lines)]
      (let [ip-match (re-matches ip-pattern line)
            instr-match (re-matches instr-pattern line)]
        (cond
         ip-match (recur (rest lines) (value-of (second ip-match)) instrs)
         instr-match (recur
                      (rest lines)
                      ip
                      (conj instrs
                            (apply vector (second instr-match) (map #(value-of (nth instr-match %)) '(2 3 4)))))))
      [ip instrs])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn step-program [ip-reg instrs vm ip]
  (if (< ip (count instrs))
    (let [[instr & args] (nth instrs ip)
          vm' (apply (instr/instructions instr) (assoc-in vm [:regs ip-reg] ip) args)]
      [vm' (inc (get-in vm' [:regs ip-reg]))])
    nil))

(defn run-program [ip-reg instrs start-vm]
  (loop [vm start-vm
         ip 0]
    (if-let [result (step-program ip-reg instrs vm ip)]
      (let [[vm' ip'] result]
        (recur vm' ip'))
      [vm ip])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def A2-part-1 (+ (* 4 19 11) (* 6 22) 9))

(def A2-part-2 (+ A2-part-1 (* (+ (* 27 28) 29) 30 14 32)))

(defn simplified-program [n]
  (reduce + (for [divisor (range 1 (inc n))
                  :when (zero? (rem n divisor))]
              divisor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def vm-0 {:regs [0 0 0 0 0 0]})
(def vm-1 {:regs [1 0 0 0 0 0]})

(defsolution day19 [input]
  ;; (let [[ip-reg instrs] (parse-input input)]
  ;;   [(get-in (run-program ip-reg instrs vm-0) [0 :regs 0])
  ;;    (get-in (run-program ip-reg instrs vm-1) [0 :regs 0])])
  [(simplified-program A2-part-1)
   (simplified-program A2-part-2)])
