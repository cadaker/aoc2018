(ns day01)

(defn find-freq-reached-twice [in-diffs]
  (loop [freq 0, diffs in-diffs, seen #{0}]
    (if (seq diffs)
      (let [freq' (+ freq (first diffs))]
        (if (contains? seen freq')
          freq'
          (recur freq' (rest diffs) (conj seen freq'))))
      (recur freq in-diffs seen))))

(defn -main []
  (let [diffs (map #(Long/valueOf %) (line-seq (java.io.BufferedReader. *in*)))
        freq (reduce + 0 diffs)]
    (println freq)
    (println (find-freq-reached-twice diffs))))
