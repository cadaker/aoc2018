(ns aoc2018.day16
  (:use aoc2018.driver
        [aoc2018.instr :as instr]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn value-of [^String s]
  (Integer/valueOf s))

(defn nth-value [coll n]
  (value-of (nth coll n)))

(def pattern1 #"Before: \[(\d+), (\d+), (\d+), (\d+)]\n(\d+) (\d+) (\d+) (\d+)\nAfter:  \[(\d+), (\d+), (\d+), (\d+)]")

(defn parse-entry-1 [input]
  (let [match (re-matches pattern1 input)]
    {:pre  (vec (map (partial nth-value match) '(1 2 3 4)))
     :op   (vec (map (partial nth-value match) '(5 6 7 8)))
     :post (vec (map (partial nth-value match) '(9 10 11 12)))}))

(def pattern2 #"(\d+) (\d+) (\d+) (\d+)")

(defn parse-entry-2 [input]
  (let [match (re-matches pattern2 input)]
    (vec (map (partial nth-value match) '(1 2 3 4)))))

(defn parse-input [input]
  (let [[part1 part2] (clojure.string/split input #"\n\n\n\n")
        entries1 (clojure.string/split part1 #"\n\n")
        entries2 (clojure.string/split part2 #"\n")]
    [(map parse-entry-1 entries1)
     (map parse-entry-2 entries2)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn possible-instructions [entry]
  (let [vm-pre {:regs (:pre entry)}
        operands (rest (:op entry))
        vm-post {:regs (:post entry)}]
    (map first (filter (fn [[name func]]
                         (= vm-post (apply func vm-pre operands)))
                       instr/instructions))))

(defn constrain-op [opcodes entry]
  (let [possible (possible-instructions entry)
        code (first (:op entry))]
    (update opcodes code (partial clojure.set/intersection (set possible)))))

(defn eliminate-op [opcodes [exclude-code op]]
  (reduce (fn [opcodes [code ops]]
            (if (not= code exclude-code)
              (assoc opcodes code (disj ops op))
              opcodes))
          opcodes
          opcodes))

(defn eliminate-codes [opcodes]
  (let [singles (for [[code ops] opcodes :when (= 1 (count ops))]
                  [code (first ops)])
        opcodes' (reduce eliminate-op opcodes singles)]
    (if (= opcodes opcodes')
      opcodes
      (recur opcodes'))))

(defn finalize-opcodes [opcodes]
  (if (every? #(= 1 (count %)) (vals opcodes))
    (into {} (for [[code ops] opcodes] [code (first ops)]))
    nil))

(defn run-instr [opcodes vm codes]
  (let [instr (instr/instructions (opcodes (first codes)))]
    (apply instr vm (rest codes))))

(def init-vm {:regs [0 0 0 0]})

(defn run-program [opcodes program]
  (reduce (partial run-instr opcodes) init-vm program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def opcode-init
  (let [all-instrs (set (keys instr/instructions))]
    (into {} (for [code (range (count all-instrs))]
               [code all-instrs]))))

(defsolution day16 [input]
  (let [[part1 part2] (parse-input input)]
    [(->> part1
          (map possible-instructions)
          (filter #(>= (count %) 3))
          count)
     (let [filtered (reduce constrain-op opcode-init part1)
           opcodes (finalize-opcodes (eliminate-codes filtered))]
       (first (:regs (run-program opcodes part2))))]))
