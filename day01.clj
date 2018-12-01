(ns day01)

(defn -main []
  (let [diffs (map #(Long/valueOf %) (line-seq (java.io.BufferedReader. *in*)))
        freq (reduce + 0 diffs)]
    (println freq)))

