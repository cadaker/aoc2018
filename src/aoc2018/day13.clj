(ns aoc2018.day13
  (:require clojure.set)
  (:use aoc2018.driver))

(defn parse-line [y line]
  (loop [cs (seq line)
         x 0
         rails {}
         carts #{}]
    (if (empty? cs)
      [rails carts]
      (case (first cs)
        \^ (recur (rest cs) (inc x) (assoc rails [x y] \|) (conj carts [x y :up :left]))
        \v (recur (rest cs) (inc x) (assoc rails [x y] \|) (conj carts [x y :down :left]))
        \< (recur (rest cs) (inc x) (assoc rails [x y] \-) (conj carts [x y :left :left]))
        \> (recur (rest cs) (inc x) (assoc rails [x y] \-) (conj carts [x y :right :left]))
        (\- \| \+ \\ \/) (recur (rest cs) (inc x) (assoc rails [x y] (first cs)) carts)
        (recur (rest cs) (inc x) rails carts)))))

(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)
        parsed-lines (map-indexed parse-line lines)
        rails (apply merge (map first parsed-lines))
        carts (apply clojure.set/union (map second parsed-lines))]
    [rails carts]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cart-coords [[x y _ _]]
  [x y])

(defn cart-collisions [cart carts]
  (filter #{(cart-coords cart)} (map cart-coords carts)))

(defn turn-cart [dir turn]
  [(case [dir turn]
       [:left :left] :down
       [:left :straight] :left
       [:left :right] :up
       [:right :left] :up
       [:right :straight] :right
       [:right :right] :down
       [:up :left] :left
       [:up :straight] :up
       [:up :right] :right
       [:down :left] :right
       [:down :straight] :down
       [:down :right] :left)
   (case turn
     :left :straight
     :straight :right
     :right :left)])

(defn update-cart [x y dir turn]
  (case dir
    :left [(dec x) y dir turn]
    :right [(inc x) y dir turn]
    :up [x (dec y) dir turn]
    :down [x (inc y) dir turn]))

(defn tick-cart [[x y dir turn] rail]
  (case rail
    \- (case dir
         :left  (update-cart x y dir turn)
         :right (update-cart x y dir turn))
    \| (case dir
         :up    (update-cart x y dir turn)
         :down  (update-cart x y dir turn))
    \/ (case dir
         :right (update-cart x y :up turn)
         :left  (update-cart x y :down turn)
         :up    (update-cart x y :right turn)
         :down  (update-cart x y :left turn))
    \\ (case dir
         :right (update-cart x y :down turn)
         :left  (update-cart x y :up turn)
         :up    (update-cart x y :left turn)
         :down  (update-cart x y :right turn))
    \+ (let [[dir' turn'] (turn-cart dir turn)]
         (update-cart x y dir' turn'))))

(defn order-carts [carts]
  (sort-by (fn [[x y _ _]] [y x]) carts))

(defn has-collision? [cart carts]
  (let [coord (cart-coords cart)
        coords (map cart-coords carts)]
    ((set coords) coord)))

(defn eliminate-carts [carts [x y]]
  (filter (fn [[x' y' _ _]]
            (not (and (= x x') (= y y'))))
          carts))

(defn step-carts [rails carts]
  (loop [carts-left (order-carts carts)
         carts-done ()
         collisions ()]
    (if (seq carts-left)
      (let [cart (first carts-left)
            [x y _ _] cart
            cart' (tick-cart cart (rails [x y]))
            collision-present (has-collision? cart' (concat (rest carts-left) carts-done))
            carts-left' (if collision-present
                          (eliminate-carts (rest carts-left) (cart-coords cart'))
                          (rest carts-left))
            carts-done' (if collision-present
                          (eliminate-carts (cons cart' carts-done) (cart-coords cart'))
                          (cons cart' carts-done))]
        (recur carts-left'
               carts-done'
               (if collision-present
                 (cons (cart-coords cart') collisions)
                 collisions)))
      [(set carts-done) collisions])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-first-crashes [rails carts]
  (let [steps (iterate (fn [[carts colls]]
                         (step-carts rails carts))
                       [carts ()])]
    (first (drop-while empty? (map second steps)))))

(defn run-carts [rails carts]
  (loop [carts carts]
    (if (< (count carts) 2)
      carts
      (recur (first (step-carts rails carts))))))

(defsolution day13 [input]
  (let [[rails carts] (parse-input input)]
    [(find-first-crashes rails carts)
     (run-carts rails carts)]))
