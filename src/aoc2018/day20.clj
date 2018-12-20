(ns aoc2018.day20
  (:use aoc2018.driver))

(def empty-graph {})

(defn add-edge [graph node node']
  (-> graph
      (update node #(conj (or % #{}) node'))
      (update node' #(conj (or % #{}) node))))

(defn move [dir [y x]]
  (case dir
    \N [(dec y) x]
    \S [(inc y) x]
    \E [y (inc x)]
    \W [y (dec x)]))

(defn has-edge? [graph node node']
  (get-in graph [node node'] nil))

(defn format-nodes [graph y]
  (let [minx (apply min (map second (keys graph)))
        maxx (apply max (map second (keys graph)))
        xs (range minx (inc maxx))
        doors (map (fn [[x x']] (if (has-edge? graph [y x] [y x']) \| \#)) (partition 2 1 xs))]
    (clojure.string/join (concat (interleave (repeat \.) doors) [\.]))))

(defn format-edges [graph y]
  (let [minx (apply min (map second (keys graph)))
        maxx (apply max (map second (keys graph)))
        xs (range minx (inc maxx))
        doors (map (fn [x] (if (has-edge? graph [y x] [(inc y) x]) \- \#)) xs)]
    (clojure.string/join (butlast (interleave doors (repeat \#))))))
  
(defn format-graph [graph]
  (let [miny (apply min (map first (keys graph)))
        maxy (apply max (map first (keys graph)))]
    (loop [y miny
           lines []]
      (if (< y maxy)
        (recur (inc y)
               (-> lines
                   (conj (format-nodes graph y))
                   (conj (format-edges graph y))))
        (clojure.string/join "\n" (conj lines (format-nodes graph y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare parse)

(defn branch [graph positions input]
  (loop [graph graph
         branches []
         input input]
    (let [[graph' positions' input'] (parse graph positions input)
          branches' (conj branches positions')]
      (case (first input')
            \| (recur graph' branches' (rest input'))
            \) [graph' (reduce clojure.set/union branches') (rest input')]))))

(defn parse [graph positions input]
  (case (first input)
    (\$ \| \)) [graph positions input]
    (\N \S \E \W) (let [positions' (map (partial move (first input)) positions)]
                    (recur (reduce (fn [graph [node node']]
                                     (add-edge graph node node'))
                                   graph
                                   (map vector positions positions'))
                           (set positions')
                           (rest input)))
    \( (let [[graph' positions' input'] (branch graph positions (rest input))]
         (recur graph' positions' input'))))

(defn build-graph [input]
  (first (parse empty-graph #{[0 0]} (rest input))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsolution day20 [input]
  (let [graph (build-graph input)]
    [(println (format-graph graph))
     0]))
