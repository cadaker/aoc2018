(ns aoc2018.day16
  (:use aoc2018.driver))

(def instructions
  {})

(defmacro definstr [name [a-type b-type] & clauses]
  (let [A (gensym "A")
        B (gensym "B")
        vm (gensym "vm")]
    `(let [func# (defn ~name [~vm ~A ~B C#]
                   (let [~'A ~(cond (= a-type :val) A (= a-type :reg) `(get-in ~vm [:regs ~A]))
                         ~'B ~(cond (= b-type :val) B (= b-type :reg) `(get-in ~vm [:regs ~B]))
                         result# ~@clauses]
                     (assoc-in ~vm [:regs C#] result#)))]
       (alter-var-root #'instructions
                       assoc (name '~name) func#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn boolval [b]
  (if b 1 0))

(definstr addr [:reg :reg]
  (+ A B))
(definstr addi [:reg :val]
  (+ A B))

(definstr mulr [:reg :reg]
  (* A B))
(definstr muli [:reg :val]
  (* A B))

(definstr banr [:reg :reg]
  (bit-and A B))
(definstr bani [:reg :val]
  (bit-and A B))

(definstr borr [:reg :reg]
  (bit-or A B))
(definstr bori [:reg :val]
  (bit-or A B))

(definstr setr [:reg :val]
  A)
(definstr seti [:val :val]
  A)

(definstr gtir [:val :reg]
  (boolval (> A B)))
(definstr gtri [:reg :val]
  (boolval (> A B)))
(definstr gtrr [:reg :reg]
  (boolval (> A B)))

(definstr eqir [:val :reg]
  (boolval (= A B)))
(definstr eqri [:reg :val]
  (boolval (= A B)))
(definstr eqrr [:reg :reg]
  (boolval (= A B)))

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

(defn parse-input [input]
  (let [[part1 part2] (clojure.string/split input #"\n\n\n\n")
        entries1 (clojure.string/split part1 #"\n\n")]
    [(map parse-entry-1 entries1) nil]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn possible-instructions [entry]
  (let [vm-pre {:regs (:pre entry)}
        operands (rest (:op entry))
        vm-post {:regs (:post entry)}]
    (map first (filter (fn [[name func]]
                         (= vm-post (apply func vm-pre operands)))
                       instructions))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def opcode-init
  (let [all-instrs (set (keys instructions))]
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
       opcodes)]))