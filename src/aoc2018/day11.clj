(ns aoc2018.day11
  (:use aoc2018.driver))

(defn abs [n]
  (if (> n 0)
    n
    (- n)))

(defn hundred-digit [n]
  (abs (rem (quot n 100) 10)))

(defn power-level [serial [x y]]
  (let [rack-id (+ x 10)
        power-1 (+ (* y rack-id) serial)
        power-2 (* power-1 rack-id)
        power-3 (hundred-digit power-2)]
    (- power-3 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn grid-around [w h [x y]]
  (for [x' (range w), y' (range h)]
    [(+ x x') (+ y y')]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn integral-image [func w h]
  (let [xys (for [x (range 0 (inc w)), y (range 0 (inc h))]
              [x y])
        int-img (reduce (fn [img [x y]]
                          (assoc img
                            [x y]
                            (if (or (zero? x) (zero? y))
                              0
                              (+' (func [x y])
                                  (- (img [(dec x) (dec y)]))
                                  (img [x (dec y)])
                                  (img [(dec x) y])))))
                        {}
                        xys)]
    int-img))

(defn img-grid-sum [img w h [x' y']]
  (let [x (dec x')
        y (dec y')
        xw (+ x w)
        yh (+ y h)]
    (+ (img [xw yh])
       (img [x y])
       (- (img [xw y]))
       (- (img [x yh])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def serial-number 7989)

(def width 300)
(def height 300)

(defn power-of-grid [serial w h xy]
  (reduce + (map (partial power-level serial)
                 (grid-around w h xy))))

(defsolution day11 [_]
  [(let [xys (for [x (range 1 (- width 1)), y (range 1 (- height 1))]
               [x y])]
     (apply max-key (partial power-of-grid serial-number 3 3) xys))
   (let [xyss (for [size (range 1 (inc (min width height)))
                    x (range 1 width)
                    y (range 1 height)
                    :when (and (<= (+ x size) width)
                               (<= (+ y size) height))]
                [x y size])
         int-img (integral-image (partial power-level serial-number) width height)]
     (apply max-key (fn [[x y size]]
                      (img-grid-sum int-img size size [x y]))
            xyss))])
