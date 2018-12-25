(ns aoc2018.union-find)

(def empty {})

(defn make-set [uf x]
  (assoc uf x {:parent x :rank 0}))

(defn- parent [uf x]
  (get-in uf [x :parent]))

(defn find [uf x]
  (loop [uf uf, y x]
    (let [p (parent uf (parent uf y))
          uf' (assoc-in uf [y :parent] p)]
      (if (= p y)
        [(assoc-in uf' [x :parent] y) y]
        (recur uf' p)))))

(defn join [uf x y]
  (let [[uf px] (find uf x)
        [uf py] (find uf y)
        rank-x (get-in uf [px :rank])
        rank-y (get-in uf [py :rank])]
    (cond
     (= px py) uf
     (< rank-x rank-y) (assoc-in uf [px :parent] py)
     (< rank-y rank-x) (assoc-in uf [py :parent] px)
     :else (-> uf
               (assoc-in [px :parent] py)
               (update-in [py :rank] inc)))))

