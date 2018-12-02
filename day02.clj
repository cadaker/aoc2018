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

(defn count-if [pred & seqs]
  (count (filter #(and %) (apply map pred seqs))))

(defn id-match [id1 id2]
  (= 1 (count-if not= id1 id2)))

(defn get-common [seq1 seq2]
  (for [[x1 x2] (map vector seq1 seq2)
        :when (= x1 x2)]
    x1))

(defn -main []
  (let [ids (line-seq (java.io.BufferedReader. *in*))
        freqs (map frequencies ids)
        [count-2 count-3] (compute-factors freqs)]
    (println (* count-2 count-3))
    (let [matches (for [id1 ids, id2 ids, :when (id-match id1 id2)]
                    (clojure.string/join (get-common id1 id2)))]
      (println (first matches)))))
