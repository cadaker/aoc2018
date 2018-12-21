(ns aoc2018.day21
  (:use aoc2018.driver))

(defn inner-loop [R3 R4]
  (let [R4* (-> R4
                (+ (bit-and R3 0xFF))
                (bit-and 0xFFFFFF)
                (* 65899)
                (bit-and 0xFFFFFF))]
    (if (< R3 256)
      R4*
      (recur (quot R3 256) R4*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def R3-init 65536)
(def R4-init 10283511)

(defsolution day21 [_]
  [(inner-loop R3-init R4-init)
   0])
