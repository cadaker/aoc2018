(ns aoc2018.cycle)

(defn find-cycle [xs]
  "Go through the list xs, returning the first [i j] such that (= (nth xs i) (nth xs j))"
  (loop [seen {}
         xs xs
         i 0]
    (if-let [first-seen-index (seen (first xs))]
      [first-seen-index i]
      (recur (assoc seen (first xs) i) (rest xs) (inc i)))))

(defn eliminate-cycle [cycle-start cycle-end n]
  "Break out cycles from a big number. Assume that n is the number of iterations of some
process, that cycles between iteration cycle-start and cycle-end
  (= (nth states cycle-start) (nth states cycle-end))
Return [equiv-iter cycle-count], where
  (= (nth states equiv-iter) (nth states n))
and cycle-count is the number of cycles taken for the full n iterations."
    (let [cycle-length (- cycle-end cycle-start)
        cycle-counts (quot (- n cycle-start) cycle-length)
        remainder (rem (- n cycle-start) cycle-length)]
    [(+ cycle-start remainder) cycle-counts]))
