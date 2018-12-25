(ns aoc2018.union_find_test
  (:require [clojure.test :refer :all])
  (:require [aoc2018.union-find :as u]))

(defn parent [uf x]
  (get-in uf [x :parent]))

(defn height [uf x]
  (loop [h 0, y x]
    (let [p (parent uf y)]
      (if (= y p)
        h
        (recur (inc h) p)))))

(deftest path-compression-test
  (let [uf (assoc-in (reduce (fn [uf [a b]]
                               (assoc uf b {:parent a}))
                             {}
                             (partition 2 1 [:a :b :c :d :e]))
                     [:a :parent]
                     :a)
        [uf' p] (u/find uf :e)]
    (testing "path-compression"
      (is (= (height uf :e) 4))
      (is (= (height uf' :e) 1)))
    (testing "path-halving"
      (is (= (height uf' :c) 1)))))

(deftest rank-test
  (let [uf {:a {:parent :a :rank 1}
            :b {:parent :b :rank 2}
            :c {:parent :c :rank 2}}]
    (testing "rank"
      (is (= (u/join uf :a :b)
             {:a {:parent :b :rank 1}
              :b {:parent :b :rank 2}
              :c {:parent :c :rank 2}}))
      (is (= (u/join uf :b :a)
             {:a {:parent :b :rank 1}
              :b {:parent :b :rank 2}
              :c {:parent :c :rank 2}}))
      (is (= (u/join uf :b :c)
             {:a {:parent :a :rank 1}
              :b {:parent :c :rank 2}
              :c {:parent :c :rank 3}})))))
