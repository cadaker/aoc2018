(ns aoc2018.day07
  (:use aoc2018.driver))

(def input-pattern #"Step (.) must be finished before step (.) can begin\.")

(defn parse-input [coll line]
  (let [[_ dependency dependant] (re-matches input-pattern line)]
    (update coll dependant (partial cons dependency))))

(defn all-nodes [graph]
  (sort (flatten (seq graph))))

(defn remove-from-graph [graph node]
  (let [graph' (dissoc graph node)]
    (reduce (fn [graph n]
              (update graph n #(remove #{node} %)))
            graph'
            (keys graph'))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn has-no-dependencies? [node graph]
  (empty? (graph node)))

(defn topo-sort [graph-in]
  (loop [graph graph-in, ordering ()]
    (let [nodes (set (all-nodes graph))
          independent-nodes (filter #(has-no-dependencies? % graph) nodes)]
      (if (seq independent-nodes)
        (let [chosen-node (first (sort independent-nodes))]
          (recur (remove-from-graph graph chosen-node)
                 (cons chosen-node ordering)))
        (reverse ordering)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-job-queue []
  {:t 0, :elems {}})

(defn job-queue-empty? [q]
  (empty? (:elems q)))

(defn jobs-queued [q]
  (set (keys (:elems q))))

(defn job-queue-size [q]
  (count (:elems q)))

(defn enqueue-job [q job time]
  (assoc-in q [:elems job] (+ (:t q) time)))

(defn dequeue-done [q]
  (let [jobs (for [[job time] (:elems q)
                   :when (>= (:t q) time)]
               job)
        elems' (apply dissoc (:elems q) jobs)]
    [(assoc q :elems elems')
     jobs]))

(defn work-job-queue [q]
  (update q :t inc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn job-time [extra job]
  (+ (- (int (first job)) (int \A))
     1
     extra))

(defn work-elves [job-graph elf-count job-time-fn]
  (loop [q' (make-job-queue)
         job-graph job-graph]
    (let [[q finished-jobs] (dequeue-done q')
          all-jobs (all-nodes job-graph)
          ;; Available jobs are jobs with no dependencies,
          ;; that are not being worked on already
          available-jobs (sort (apply disj
                                      (set (filter #(has-no-dependencies? % job-graph)
                                                   all-jobs))
                                      (jobs-queued q)))]

      (cond
       ;; If any jobs were done, remove them from our job list and start over
       (seq finished-jobs)
       (recur q
              (reduce remove-from-graph
                      job-graph
                      finished-jobs))
       ;; If we have finished all jobs, and no more to do, then we're done
       (and (job-queue-empty? q)
            (empty? all-jobs))
       (:t q)
       ;; If we have a job ready, and an elf ready, then queue a new job
       (and (not= (job-queue-size q) elf-count)
            (seq available-jobs))
       (let [job (first available-jobs)]
         (recur (enqueue-job q job (job-time-fn job))
                job-graph))
       ;; Otherwise, just keep on working
       :else
       (recur (work-job-queue q)
              job-graph)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsolution day07 [input]
  (let [graph (reduce parse-input {} (clojure.string/split-lines input))]
    [(clojure.string/join (topo-sort graph))
     (work-elves graph 5 (partial job-time 60))]))

