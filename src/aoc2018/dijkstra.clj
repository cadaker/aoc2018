(ns aoc2018.dijkstra)

(def empty-dijkstra {:lookup {} :boundary (sorted-set)})

(defn dijkstra-peek [dij]
  "Return [d state] for the unvisited state with smallest d."
  (first (:boundary dij)))

(defn dijkstra-pop [dij]
  "Remove the closest state (the one that would have been returned by dijkstra-peek)"
  (let [[d state] (dijkstra-peek dij)]
    {:lookup (dissoc (:lookup dij) state)
     :boundary (disj (:boundary dij) [d state])}))

(defn dijkstra-update [dij d state]
  "Update the entry for state to be d, if d is smaller than the current distance."
  (let [cur-d (get-in dij [:lookup state] nil)
        boundary-tmp (disj (:boundary dij) [cur-d state])]
    (if (or (nil? cur-d)
            (< d cur-d))
      {:lookup (assoc (:lookup dij) state d)
       :boundary (conj (disj (:boundary dij) [cur-d state]) [d state])}
      dij)))


(defn do-dijkstra [start-state end-state steps-fn steps-state]
  (loop [dij (dijkstra-update empty-dijkstra 0 start-state)
         visited? #{}
         steps-state steps-state]
    (let [[cur-d cur-state] (dijkstra-peek dij)
          visited? (conj visited? cur-state)]
      (if (= cur-state end-state)
        cur-d
        (let [[steps-state' all-steps] (steps-fn steps-state cur-state)
              steps (remove (comp visited? second) all-steps)]
          (recur (reduce (fn [dij [d state]]
                           (dijkstra-update dij (+ cur-d d) state))
                         (dijkstra-pop dij)
                         steps)
                 visited?
                 steps-state'))))))
