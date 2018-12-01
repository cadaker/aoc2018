(ns day01)

(defn -main []
  (let [xs (map #(Long/valueOf %) (line-seq (java.io.BufferedReader. *in*)))
        freq (reduce + 0 xs)]
    (println freq)))

