(ns aoc2018.instr)

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
