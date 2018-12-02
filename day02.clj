(ns day02)

(defn letter-count-modifier [freq n]
  (if (some (partial = n) (vals freq))
    1
    0))

(defn compute-factors [freqs]
  (reduce (fn [[count-2 count-3] freq]
            [(+ count-2 (letter-count-modifier freq 2))
             (+ count-3 (letter-count-modifier freq 3))])
          [0 0]
          freqs))

(defn -main []
  (let [ids (line-seq (java.io.BufferedReader. *in*))
        freqs (map frequencies ids)
        [count-2 count-3] (compute-factors freqs)]
    (println (* count-2 count-3))))
