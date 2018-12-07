(ns aoc2018.day07
  (:use aoc2018.driver))

(def input-pattern #"Step (.) must be finished before step (.) can begin\.")

(defn parse-input [coll line]
  (let [[_ dependency dependant] (re-matches input-pattern line)]
    (update coll dependant (partial cons dependency))))

(defn all-nodes [graph]
  (sort (flatten (seq graph))))

(defn remove-from-graph [node graph]
  (let [graph' (dissoc graph node)]
    (reduce (fn [graph n]
              (update graph n #(remove #{node} %)))
            graph'
            (keys graph'))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn has-no-dependencies? [node graph]
  (empty? (graph node)))

(defn topo-sort [graph-in]
  (loop [nodes (set (all-nodes graph-in)), graph graph-in, ordering ()]
    (let [independent-nodes (filter #(has-no-dependencies? % graph) nodes)]
      (if (seq independent-nodes)
        (let [chosen-node (first (sort independent-nodes))]
          (recur (disj nodes chosen-node)
                 (remove-from-graph chosen-node graph)
                 (cons chosen-node ordering)))
        (reverse ordering)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsolution day07 [input]
  (let [graph (reduce parse-input {} (clojure.string/split-lines input))]
    [(clojure.string/join (topo-sort graph))
     0]))

