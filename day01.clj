(ns day01)

(defn find-freq-reached-twice [in-diffs]
  (loop [freq 0, diffs (cycle in-diffs), seen #{0}]
    (let [freq' (+ freq (first diffs))]
      (if (contains? seen freq')
        freq'
        (recur freq' (rest diffs) (conj seen freq'))))))

(defn -main []
  (let [diffs (map #(Long/valueOf %) (line-seq (java.io.BufferedReader. *in*)))
        freq (reduce + 0 diffs)]
    (println freq)
    (println (find-freq-reached-twice diffs))))
