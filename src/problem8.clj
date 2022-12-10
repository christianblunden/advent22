(ns problem8
  (:require [clojure.core.matrix :as m]))

(m/set-current-implementation :ndarray)

(defn load-data [f n] (->> f slurp (re-seq #"\d") (map read-string) (partition n) m/matrix))
;; (def input (load-data "resources/input8-example.txt" 5))
(def input (load-data "resources/input8.txt" 99))

(defn visible? [[dim i n]]
  (let [xs (m/slice-view input dim i)
        [l [x & r]] (split-at n xs)]
    [(< (apply max l) x) (< (apply max r) x)]))

(defn any-visble? [[col row]]
  (->> [[0 row col] [1 col row]]
       (map visible?)
       flatten
       (not-every? false?)))

(defn boundary? [i s]
  (not-every? true? (map #(< 0 % %2) i s)))

(defn count-trees [input]
  (let [s (->> (m/shape input) (map dec))]
    (->> input
         (m/emap-indexed (fn [i _] (if (or (boundary? i s) (any-visble? i)) 1 0)))
         (m/ereduce +))))

;; part 1 "Elapsed time: 900.350163 msecs"
(time (count-trees input))

;; part2
(defn take-until [x xs]
  (reduce (fn [agg i]
            (if (< i x)
              (conj agg i)
              (reduced (conj agg i))))
          []
          xs))

 (defn count-trees2 [[dim i n]]
  (let [xs (m/slice-view input dim i)
        [l [x & r]] (split-at n xs)]
    [(count (take-until x (reverse l))) 
     (count (take-until x r))]))

(defn viewing-score [[col row] _]
  (->> [[0 row col] [1 col row]]
       (map count-trees2)
       flatten
       (reduce *)))

;; part 2 "Elapsed time: 802.626502 msecs"
(time (m/emax (m/emap-indexed viewing-score input)))