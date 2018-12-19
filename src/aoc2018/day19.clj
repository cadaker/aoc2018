(ns aoc2018.day19
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
          vm' (apply (instructions instr) (assoc-in vm [:regs ip-reg] ip) args)]
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

(def vm-0 {:regs [0 0 0 0 0 0]})

(defsolution day19 [input]
  (let [[ip-reg instrs] (parse-input input)]
    [(get-in (run-program ip-reg instrs vm-0) [0 :regs 0])
     0]))
